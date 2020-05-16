// This file contains the common code every implementation uses for stage2
// It is intended to be included multiple times and compiled multiple times
// We assume the file in which it is include already includes
// "simdjson/stage2_build_tape.h" (this simplifies amalgation)

namespace stage2 {

namespace {

#ifdef SIMDJSON_USE_COMPUTED_GOTO
typedef void* ret_address;
#define INIT_ADDRESSES() { &&array_begin, &&array_continue, &&error, &&finish, &&object_begin, &&object_continue }
#define GOTO(address) { goto *(address); }
#define CONTINUE(address) { goto *(address); }
#else
typedef char ret_address;
#define INIT_ADDRESSES() { '[', 'a', 'e', 'f', '{', 'o' };
#define GOTO(address)                 \
  {                                   \
    switch(address) {                 \
      case '[': goto array_begin;     \
      case 'a': goto array_continue;  \
      case 'e': goto error;           \
      case 'f': goto finish;          \
      case '{': goto object_begin;    \
      case 'o': goto object_continue; \
    }                                 \
  }
// For the more constrained end_xxx() situation
#define CONTINUE(address)             \
  {                                   \
    switch(address) {                 \
      case 'a': goto array_continue;  \
      case 'o': goto object_continue; \
      case 'f': goto finish;          \
    }                                 \
  }
#endif

struct unified_machine_addresses {
  ret_address array_begin;
  ret_address array_continue;
  ret_address error;
  ret_address finish;
  ret_address object_begin;
  ret_address object_continue;
};

#undef FAIL_IF
#define FAIL_IF(EXPR) { if (EXPR) { return addresses.error; } }

struct number_writer {
  parser &doc_parser;
  
  really_inline void write_s64(int64_t value) noexcept {
    write_tape(0, internal::tape_type::INT64);
    std::memcpy(&doc_parser.doc.tape[doc_parser.current_loc], &value, sizeof(value));
    ++doc_parser.current_loc;
  }
  really_inline void write_u64(uint64_t value) noexcept {
    write_tape(0, internal::tape_type::UINT64);
    doc_parser.doc.tape[doc_parser.current_loc++] = value;
  }
  really_inline void write_double(double value) noexcept {
    write_tape(0, internal::tape_type::DOUBLE);
    static_assert(sizeof(value) == sizeof(doc_parser.doc.tape[doc_parser.current_loc]), "mismatch size");
    memcpy(&doc_parser.doc.tape[doc_parser.current_loc++], &value, sizeof(double));
    // doc.tape[doc.current_loc++] = *((uint64_t *)&d);
  }
  really_inline void write_tape(uint64_t val, internal::tape_type t) noexcept {
    doc_parser.doc.tape[doc_parser.current_loc++] = val | ((uint64_t(char(t))) << 56);
  }
}; // struct number_writer

struct structural_parser {
  const uint8_t *const buf;
  parser &doc_parser;
  const uint32_t *&next_structural_index;
  uint8_t *&current_string_buf_loc;
  uint32_t depth;

  really_inline structural_parser(
    const uint8_t *_buf,
    parser &_doc_parser,
    const uint32_t *&_next_structural_index,
    uint8_t *&_current_string_buf_loc,
    uint32_t _depth
  ) : buf{_buf},
      doc_parser{_doc_parser},
      next_structural_index{_next_structural_index},
      current_string_buf_loc{_current_string_buf_loc},
      depth{_depth} {
  }

  really_inline void start_document() {
    write_tape(0, internal::tape_type::ROOT); // if the document is correct, this gets rewritten later
  }
  really_inline void end_document() {
    doc_parser.doc.tape[0] |= doc_parser.current_loc | (uint64_t(1) << 32);
    write_tape(0, internal::tape_type::ROOT);
  }

  WARN_UNUSED really_inline bool start_scope(internal::tape_type type, ret_address continue_state) {
    doc_parser.containing_scope[depth].tape_index = doc_parser.current_loc;
    doc_parser.containing_scope[depth].count = 0;
    write_tape(0, type); // if the document is correct, this gets rewritten later
    doc_parser.ret_address[depth] = continue_state;
    depth++;
    return depth >= doc_parser.max_depth();
  }

  WARN_UNUSED really_inline bool start_object(ret_address continue_state) {
    return start_scope(internal::tape_type::START_OBJECT, continue_state);
  }

  WARN_UNUSED really_inline bool start_array(ret_address continue_state) {
    return start_scope(internal::tape_type::START_ARRAY, continue_state);
  }

  // this function is responsible for annotating the start of the scope
  really_inline void end_scope(internal::tape_type type) noexcept {
    depth--;
    // write our doc.tape location to the header scope
    // The root scope gets written *at* the previous location.
    write_tape(doc_parser.containing_scope[depth].tape_index, type);
    // count can overflow if it exceeds 24 bits... so we saturate
    // the convention being that a cnt of 0xffffff or more is undetermined in value (>=  0xffffff).
    const uint32_t start_tape_index = doc_parser.containing_scope[depth].tape_index;
    const uint32_t count = doc_parser.containing_scope[depth].count;
    const uint32_t cntsat = count > 0xFFFFFF ? 0xFFFFFF : count;
    // This is a load and an OR. It would be possible to just write once at doc.tape[d.tape_index]
    doc_parser.doc.tape[start_tape_index] |= doc_parser.current_loc | (uint64_t(cntsat) << 32);
  }

  really_inline void end_object() {
    end_scope(internal::tape_type::END_OBJECT);
  }
  really_inline void end_array() {
    end_scope(internal::tape_type::END_ARRAY);
  }

  really_inline void write_tape(uint64_t val, internal::tape_type t) noexcept {
    doc_parser.doc.tape[doc_parser.current_loc++] = val | ((uint64_t(char(t))) << 56);
  }

  // increment_count increments the count of keys in an object or values in an array.
  // Note that if you are at the level of the values or elements, the count
  // must be increment in the preceding depth (depth-1) where the array or
  // the object resides.
  really_inline void increment_count() {
    doc_parser.containing_scope[depth - 1].count++; // we have a key value pair in the object at parser.depth - 1
  }

  really_inline uint8_t *on_start_string() noexcept {
    /* we advance the point, accounting for the fact that we have a NULL
      * termination         */
    write_tape(current_string_buf_loc - doc_parser.doc.string_buf.get(), internal::tape_type::STRING);
    return current_string_buf_loc + sizeof(uint32_t);
  }

  really_inline bool on_end_string(uint8_t *dst) noexcept {
    uint32_t str_length = uint32_t(dst - (current_string_buf_loc + sizeof(uint32_t)));
    // TODO check for overflow in case someone has a crazy string (>=4GB?)
    // But only add the overflow check when the document itself exceeds 4GB
    // Currently unneeded because we refuse to parse docs larger or equal to 4GB.
    memcpy(current_string_buf_loc, &str_length, sizeof(uint32_t));
    // NULL termination is still handy if you expect all your strings to
    // be NULL terminated? It comes at a small cost
    *dst = 0;
    current_string_buf_loc = dst + 1;
    return true;
  }

  WARN_UNUSED really_inline bool parse_string() {
    uint8_t *dst = on_start_string();
    dst = stringparsing::parse_string(current_buf(), dst);
    if (dst == nullptr) {
      return true;
    }
    return !on_end_string(dst);
  }

  WARN_UNUSED really_inline bool parse_number(const uint8_t *src, bool found_minus) {
    number_writer writer{doc_parser};
    return !numberparsing::parse_number(src, found_minus, writer);
  }
  WARN_UNUSED really_inline bool parse_number(bool found_minus) {
    return parse_number(current_buf(), found_minus);
  }

  WARN_UNUSED really_inline bool parse_single_number(size_t len, bool found_minus) {
    /**
    * We need to make a copy to make sure that the string is space terminated.
    * This is not about padding the input, which should already padded up
    * to len + SIMDJSON_PADDING. However, we have no control at this stage
    * on how the padding was done. What if the input string was padded with nulls?
    * It is quite common for an input string to have an extra null character (C string).
    * We do not want to allow 9\0 (where \0 is the null character) inside a JSON
    * document, but the string "9\0" by itself is fine. So we make a copy and
    * pad the input with spaces when we know that there is just one input element.
    * This copy is relatively expensive, but it will almost never be called in
    * practice unless you are in the strange scenario where you have many JSON
    * documents made of single atoms.
    */
    size_t remaining_len = buf + len - current_buf();
    uint8_t *copy = static_cast<uint8_t *>(malloc(remaining_len + SIMDJSON_PADDING));
    if (copy == nullptr) {
      return true;
    }
    memcpy(copy, current_buf(), remaining_len);
    memset(copy + remaining_len, ' ', SIMDJSON_PADDING);
    bool result = parse_number(copy, found_minus);
    free(copy);
    return result;
  }

  WARN_UNUSED really_inline bool parse_atom() {
    switch (current_char()) {
      case 't':
        if (!atomparsing::is_valid_true_atom(current_buf())) { return true; }
        write_tape(0, internal::tape_type::TRUE_VALUE);
        break;
      case 'f':
        if (!atomparsing::is_valid_false_atom(current_buf())) { return true; }
        write_tape(0, internal::tape_type::FALSE_VALUE);
        break;
      case 'n':
        if (!atomparsing::is_valid_null_atom(current_buf())) { return true; }
        write_tape(0, internal::tape_type::NULL_VALUE);
        break;
      default:
        return true;
    }
    return false;
  }

  really_inline size_t remaining_len(size_t len) {
    return buf + len - current_buf();
  }

  WARN_UNUSED really_inline bool parse_single_atom(size_t len) {
    switch (current_char()) {
      case 't':
        if (!atomparsing::is_valid_true_atom(current_buf(), remaining_len(len))) { return true; }
        write_tape(0, internal::tape_type::TRUE_VALUE);
        break;
      case 'f':
        if (!atomparsing::is_valid_false_atom(current_buf(), remaining_len(len))) { return true; }
        write_tape(0, internal::tape_type::FALSE_VALUE);
        break;
      case 'n':
        if (!atomparsing::is_valid_null_atom(current_buf(), remaining_len(len))) { return true; }
        write_tape(0, internal::tape_type::NULL_VALUE);
        break;
      default:
        return true;
    }
    return false;
  }

  WARN_UNUSED really_inline ret_address parse_value(const unified_machine_addresses &addresses, ret_address continue_state) {
    switch (advance_char()) {
    case '"':
      FAIL_IF( parse_string() );
      return continue_state;
    case 't': case 'f': case 'n':
      FAIL_IF( parse_atom() );
      return continue_state;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      FAIL_IF( parse_number(false) );
      return continue_state;
    case '-':
      FAIL_IF( parse_number(true) );
      return continue_state;
    case '{':
      FAIL_IF( start_object(continue_state) );
      return addresses.object_begin;
    case '[':
      FAIL_IF( start_array(continue_state) );
      return addresses.array_begin;
    default:
      return addresses.error;
    }
  }

  WARN_UNUSED really_inline error_code error() {
    /* We do not need the next line because this is done by doc_parser.init_stage2(),
    * pessimistically.
    * doc_parser.is_valid  = false;
    * At this point in the code, we have all the time in the world.
    * Note that we know exactly where we are in the document so we could,
    * without any overhead on the processing code, report a specific
    * location.
    * We could even trigger special code paths to assess what happened
    * carefully,
    * all without any added cost. */
    if (depth >= doc_parser.max_depth()) {
      return DEPTH_ERROR;
    }
    switch (current_char()) {
    case '"':
      return STRING_ERROR;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '-':
      return NUMBER_ERROR;
    case 't':
      return T_ATOM_ERROR;
    case 'n':
      return N_ATOM_ERROR;
    case 'f':
      return F_ATOM_ERROR;
    default:
      return TAPE_ERROR;
    }
  }

  really_inline char peek_char() {
    return buf[*next_structural_index];
  }
  really_inline char advance_char() {
    // Start pulling the next iteration pointer early
    next_structural_index++;
    return current_char();
  }
  really_inline char current_char() {
    return *current_buf();
  }
  really_inline const uint8_t* current_buf() {
    return &buf[*(next_structural_index-1)];
  }
};

// Redefine FAIL_IF to use goto since it'll be used inside the function now
#undef FAIL_IF
#define FAIL_IF(EXPR) { if (EXPR) { goto error; } }

WARN_UNUSED static bool parse_value(
  const uint8_t *buf,
  parser &doc_parser,
  const uint32_t *next_structural_index,
  uint8_t *current_string_buf_loc,
  uint32_t depth
) noexcept {
  static constexpr stage2::unified_machine_addresses addresses = INIT_ADDRESSES();
  structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc, depth);

  GOTO( parser.parse_value(addresses, addresses.finish) );

  //
  // Object parser states
  //
  object_begin: {
    switch (parser.advance_char()) {
    case '"': {
      FAIL_IF( parser.parse_string() );
      goto object_key_state;
    }
    case '}':
      parser.end_object();
      goto scope_end;
    default:
      goto error;
    }
  }

  object_key_state: {
    FAIL_IF( parser.advance_char() != ':' );
    parser.increment_count();
    GOTO( parser.parse_value(addresses, addresses.object_continue) );
  }

  object_continue: {
    switch (parser.advance_char()) {
    case ',':
      FAIL_IF( parser.advance_char() != '"' );
      FAIL_IF( parser.parse_string() );
      goto object_key_state;
    case '}':
      parser.end_object();
      goto scope_end;
    default:
      goto error;
    }
  }

  scope_end: {
    CONTINUE( parser.doc_parser.ret_address[parser.depth] );
  }

  //
  // Array parser states
  //
  array_begin: {
    if (parser.peek_char() == ']') {
      parser.advance_char();
      parser.end_array();
      goto scope_end;
    }
  }

  main_array_switch: {
    // we call update char on all paths in, so we can peek at parser.c on the
    // on paths that can accept a close square brace (post-, and at start)
    parser.increment_count();
    GOTO( parser.parse_value(addresses, addresses.array_continue) );
  }

  array_continue: {
    switch (parser.advance_char()) {
    case ',':
      goto main_array_switch;
    case ']':
      parser.end_array();
      goto scope_end;
    default:
      goto error;
    }
  }

  finish: {
    return false;
  }

  error: {
    return true;
  }
} // parse_value()

WARN_UNUSED static really_inline error_code parse_document(
  const uint8_t *buf,
  parser &doc_parser,
  const uint32_t *next_structural_index,
  uint8_t *current_string_buf_loc,
  size_t len
) noexcept {
  // Initialize parsing
  structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc, 0);
  doc_parser.current_loc = 0;
  doc_parser.valid = false;
  doc_parser.error = UNINITIALIZED;

  // Push the root scope (there is always at least one scope)
  parser.start_document();

  //
  // Special case first value parsing if it's an atom or number
  //
  bool failed;
  switch (parser.peek_char()) {
  case 't': case 'f': case 'n':
    parser.advance_char();
    failed = parser.parse_single_atom(len);
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    parser.advance_char();
    failed = parser.parse_single_number(len, false);
    break;
  case '-':
    parser.advance_char();
    failed = parser.parse_single_number(len, true);
    break;
  default:
    failed = parse_value(buf, doc_parser, next_structural_index, current_string_buf_loc, parser.depth);
    break;
  }

  // Finish up the document and check for errors.
  parser.end_document();

  if (failed) {
    return doc_parser.error = parser.error();
  }

  if (parser.depth != 0) {
    return doc_parser.error = TAPE_ERROR;
  }

  doc_parser.valid = true;
  return doc_parser.error = (*next_structural_index == 0) ? SUCCESS : SUCCESS_AND_HAS_MORE;
} // parse_document()

} // namespace {}

} // namespace stage2

/************
 * The JSON is parsed to a tape, see the accompanying tape.md file
 * for documentation.
 ***********/
WARN_UNUSED error_code implementation::stage2(const uint8_t *buf, size_t len, parser &doc_parser) const noexcept {
  const uint32_t *next_structural_index = &doc_parser.structural_indexes[0];
  uint8_t *current_string_buf_loc = &doc_parser.doc.string_buf[0];
  error_code error = stage2::parse_document(buf, doc_parser, next_structural_index, current_string_buf_loc, len);
  // If there were more structurals, this is not a single valid document.
  if (error == SUCCESS_AND_HAS_MORE) {
    doc_parser.valid = false;
    return doc_parser.error = TAPE_ERROR;
  }
  return error;
}

/************
 * The JSON is parsed to a tape, see the accompanying tape.md file
 * for documentation.
 ***********/
WARN_UNUSED error_code implementation::stage2(const uint8_t *buf, size_t len, parser &doc_parser, size_t &next_json) const noexcept {
  const uint32_t *next_structural_index = &doc_parser.structural_indexes[next_json];
  uint8_t *current_string_buf_loc = &doc_parser.doc.string_buf[0];
  return stage2::parse_document(buf, doc_parser, next_structural_index, current_string_buf_loc, len);
  return SUCCESS;
}

WARN_UNUSED error_code implementation::parse(const uint8_t *buf, size_t len, parser &doc_parser) const noexcept {
  error_code code = stage1(buf, len, doc_parser, false);
  if (!code) {
    code = stage2(buf, len, doc_parser);
  }
  return code;
}
