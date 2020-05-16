namespace stage2 {

struct streaming_structural_parser: structural_parser {
  really_inline streaming_structural_parser(
    parser &_doc_parser,
    const uint8_t *_buf,
    const uint32_t *_next_structural_index,
    uint8_t *_current_string_buf_loc,
    uint32_t _depth
  ) : structural_parser(_doc_parser, _buf, _next_structural_index, _current_string_buf_loc, _depth) {
  }

  // override to add streaming
  WARN_UNUSED really_inline error_code start(UNUSED size_t len, ret_address finish_parser) {
    init(); // sets is_valid to false
    // Capacity ain't no thang for streaming, so we don't check it.
    // Advance to the first character as soon as possible
    advance_char();
    // Push the root scope (there is always at least one scope)
    if (start_document(finish_parser)) {
      return on_error(DEPTH_ERROR);
    }
    return SUCCESS;
  }

  // override to add streaming
  WARN_UNUSED really_inline error_code finish() {
    if ( next_structural_index > end_structural_indexes() ) {
      return on_error(TAPE_ERROR);
    }
    end_document();
    if (depth != 0) {
      return on_error(TAPE_ERROR);
    }
    if (doc_parser.containing_scope[depth].tape_index != 0) {
      return on_error(TAPE_ERROR);
    }
    bool finished = next_structural_index == end_structural_indexes();
    return on_success(finished ? SUCCESS : SUCCESS_AND_HAS_MORE);
  }
};

} // namespace stage2

/************
 * The JSON is parsed to a tape, see the accompanying tape.md file
 * for documentation.
 ***********/
WARN_UNUSED error_code implementation::stage2(const uint8_t *buf, size_t len, parser &doc_parser, size_t &next_json) const noexcept {
  static constexpr stage2::unified_machine_addresses addresses = INIT_ADDRESSES();
  stage2::structural_parser parser(doc_parser, buf, &doc_parser.structural_indexes[next_json], doc_parser.doc.string_buf.get(), 0);
  error_code result = parser.start(len, addresses.finish);
  if (result) { return result; }

  //
  // Read first value
  //
  switch (parser.advance_char()) {
  case '{':
    FAIL_IF( parser.start_object(addresses.finish) );
    goto object_begin;
  case '[':
    FAIL_IF( parser.start_array(addresses.finish) );
    goto array_begin;
  case '"':
    FAIL_IF( parser.parse_string() );
    goto finish;
  case 't': case 'f': case 'n':
    FAIL_IF( parser.parse_single_atom(len) );
    goto finish;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    FAIL_IF( parser.parse_single_number(len, false) );
    goto finish;
  case '-':
    FAIL_IF( parser.parse_single_number(len, true) );
    goto finish;
  default:
    goto error;
  }

//
// Object parser parsers
//
object_begin:
  switch (parser.advance_char()) {
  case '"': {
    FAIL_IF( parser.parse_string() );
    goto object_key_parser;
  }
  case '}':
    parser.end_object();
    goto scope_end;
  default:
    goto error;
  }

object_key_parser:
  FAIL_IF( parser.advance_char() != ':' );
  GOTO( parser.parse_value(addresses, addresses.object_continue) );

object_continue:
  switch (parser.advance_char()) {
  case ',':
    FAIL_IF( parser.advance_char() != '"' );
    FAIL_IF( parser.parse_string() );
    goto object_key_parser;
  case '}':
    parser.end_object();
    goto scope_end;
  default:
    goto error;
  }

scope_end:
  CONTINUE( parser.doc_parser.ret_address[parser.depth] );

//
// Array parser parsers
//
array_begin:
  if (parser.peek_char() == ']') {
    parser.advance_char();
    parser.end_array();
    goto scope_end;
  }
  parser.increment_count();

main_array_switch:
  /* we call update char on all paths in, so we can peek at parser.c on the
   * on paths that can accept a close square brace (post-, and at start) */
  GOTO( parser.parse_value(addresses, addresses.array_continue) );

array_continue:
  switch (parser.advance_char()) {
  case ',':
    parser.increment_count();
    goto main_array_switch;
  case ']':
    parser.end_array();
    goto scope_end;
  default:
    goto error;
  }

finish:
  next_json = parser.next_structural_index - parser.doc_parser.structural_indexes.get();
  return parser.finish();

error:
  return parser.error();
}
