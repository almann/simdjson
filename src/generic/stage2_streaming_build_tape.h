namespace stage2 {

struct streaming_structural_parser: structural_parser {
  really_inline streaming_structural_parser(
    const uint8_t *_buf,
    parser &_doc_parser,
    const uint32_t *&_next_structural_index,
    uint8_t *&_current_string_buf_loc,
    uint32_t _depth
  ) : structural_parser(_buf, _doc_parser, _next_structural_index, _current_string_buf_loc, _depth) {
  }

  // override to add streaming
  WARN_UNUSED really_inline error_code start(UNUSED size_t len) {
    init(); // sets is_valid to false
    // Capacity ain't no thang for streaming, so we don't check it.
    // Advance to the first character as soon as possible
    advance_char();
    // Push the root scope (there is always at least one scope)
    start_document();
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
  const uint32_t *next_structural_index = &doc_parser.structural_indexes[next_json];
  uint8_t *current_string_buf_loc = &doc_parser.doc.string_buf[0];
  error_code result = stage2::parse_document<stage2::streaming_structural_parser>(buf, doc_parser, next_structural_index, current_string_buf_loc, len);
  next_json = next_structural_index - &doc_parser.structural_indexes[0];
  return result;
}
