// This file contains the common code every implementation uses for stage2
// It is intended to be included multiple times and compiled multiple times
// We assume the file in which it is include already includes
// "simdjson/stage2_build_tape.h" (this simplifies amalgation)

namespace stage2 {

namespace {

static constexpr const bool DEBUG = false;
static constexpr const int LOG_TITLE_LEN = 30;

static int log_depth; // Not threadsafe. Log only.

struct number_writer {
  parser &doc_parser;
  
  really_inline void write_s64(int64_t value) noexcept {
    append_tape(0, internal::tape_type::INT64);
    std::memcpy(&doc_parser.doc.tape[doc_parser.current_loc], &value, sizeof(value));
    ++doc_parser.current_loc;
  }
  really_inline void write_u64(uint64_t value) noexcept {
    append_tape(0, internal::tape_type::UINT64);
    doc_parser.doc.tape[doc_parser.current_loc++] = value;
  }
  really_inline void write_double(double value) noexcept {
    append_tape(0, internal::tape_type::DOUBLE);
    static_assert(sizeof(value) == sizeof(doc_parser.doc.tape[doc_parser.current_loc]), "mismatch size");
    memcpy(&doc_parser.doc.tape[doc_parser.current_loc++], &value, sizeof(double));
    // doc.tape[doc.current_loc++] = *((uint64_t *)&d);
  }
  really_inline void append_tape(uint64_t val, internal::tape_type t) noexcept {
    doc_parser.doc.tape[doc_parser.current_loc++] = val | ((uint64_t(char(t))) << 56);
  }
}; // struct number_writer

struct structural_parser {
  const uint8_t *const buf;
  parser &doc_parser;
  const uint32_t *&next_structural_index;
  uint8_t *&current_string_buf_loc;

  really_inline structural_parser(
    const uint8_t *_buf,
    parser &_doc_parser,
    const uint32_t *&_next_structural_index,
    uint8_t *&_current_string_buf_loc
  ) : buf{_buf},
      doc_parser{_doc_parser},
      next_structural_index{_next_structural_index},
      current_string_buf_loc{_current_string_buf_loc} {
  }

  WARN_UNUSED really_inline bool parse_document(size_t len) {
    log_start();
    log_start_value("document");
    doc_parser.current_loc = 1; // we will stuff the root element at the beginning later

    // Parse the document and check for errors
    bool result = parse_root_value(len);
    // Write the begin/end root element
    append_tape(0, internal::tape_type::ROOT);
    write_tape(0, doc_parser.current_loc, internal::tape_type::ROOT);
    log_end_value("document");
    return result;
  }

  WARN_UNUSED really_inline bool parse_value() {
    switch (advance_char()) {
    case '"':
      log_value("string");
      return parse_string();
    case 't':
      return parse_true_atom();
    case 'f':
      return parse_false_atom();
    case 'n':
      return parse_null_atom();
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return parse_number(false);
    case '-':
      return parse_number(true);
    case '{':
      return parse_object();
    case '[':
      return parse_array();
    default:
      log_error("Unknown structural character!");
      return false;
    }
  }

  WARN_UNUSED really_inline bool parse_root_value(size_t len) {
    switch (advance_char()) {
    case '"':
      log_value("string");
      return parse_string();
    case 't':
      return parse_root_true_atom(len);
    case 'f':
      return parse_root_false_atom(len);
    case 'n':
      return parse_root_null_atom(len);
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return parse_root_number(len, false);
    case '-':
      return parse_root_number(len, true);
    case '{':
      return parse_object();
    case '[':
      return parse_array();
    default:
      log_error("Unknown structural character!");
      return false;
    }
  }

  WARN_UNUSED really_inline bool parse_string() {
    uint8_t *dst = start_string();
    dst = stringparsing::parse_string(current_buf(), dst);
    if (dst == nullptr) {
      log_error("failed to parse string");
      return false;
    }
    end_string(dst);
    return true;
  }

  really_inline uint8_t *start_string() noexcept {
    // we advance the point, accounting for the fact that we have a NULL termination
    append_tape(current_string_buf_loc - doc_parser.doc.string_buf.get(), internal::tape_type::STRING);
    return current_string_buf_loc + sizeof(uint32_t);
  }

  really_inline bool end_string(uint8_t *dst) noexcept {
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

  WARN_UNUSED really_inline bool parse_number(const uint8_t *src, bool found_minus) {
    log_value("number");
    number_writer writer{doc_parser};
    return numberparsing::parse_number(src, found_minus, writer);
  }
  WARN_UNUSED really_inline bool parse_number(bool found_minus) {
    return parse_number(current_buf(), found_minus);
  }

  WARN_UNUSED really_inline bool parse_root_number(size_t len, bool found_minus) {
    //
    // We need to make a copy to make sure that the string is space terminated.
    // This is not about padding the input, which should already padded up
    // to len + SIMDJSON_PADDING. However, we have no control at this stage
    // on how the padding was done. What if the input string was padded with nulls?
    // It is quite common for an input string to have an extra null character (C string).
    // We do not want to allow 9\0 (where \0 is the null character) inside a JSON
    // document, but the string "9\0" by itself is fine. So we make a copy and
    // pad the input with spaces when we know that there is just one input element.
    // This copy is relatively expensive, but it will almost never be called in
    // practice unless you are in the strange scenario where you have many JSON
    // documents made of single atoms.
    //
    size_t remaining_len = buf + len - current_buf();
    uint8_t *copy = static_cast<uint8_t *>(malloc(remaining_len + SIMDJSON_PADDING));
    if (copy == nullptr) {
      return false;
    }
    memcpy(copy, current_buf(), remaining_len);
    memset(copy + remaining_len, ' ', SIMDJSON_PADDING);
    bool result = parse_number(copy, found_minus);
    free(copy);
    return result;
  }

  WARN_UNUSED really_inline bool parse_true_atom() {
    log_value("true");
    append_tape(0, internal::tape_type::TRUE_VALUE);
    return atomparsing::is_valid_true_atom(current_buf());
  }
  WARN_UNUSED really_inline bool parse_false_atom() {
    log_value("false");
    append_tape(0, internal::tape_type::FALSE_VALUE);
    return atomparsing::is_valid_false_atom(current_buf());
  }
  WARN_UNUSED really_inline bool parse_null_atom() {
    log_value("null");
    append_tape(0, internal::tape_type::NULL_VALUE);
    return atomparsing::is_valid_null_atom(current_buf());
  }

  WARN_UNUSED really_inline bool parse_root_true_atom(size_t len) {
    log_value("true");
    append_tape(0, internal::tape_type::TRUE_VALUE);
    return atomparsing::is_valid_true_atom(current_buf(), remaining_len(len));
  }
  WARN_UNUSED really_inline bool parse_root_false_atom(size_t len) {
    log_value("false");
    append_tape(0, internal::tape_type::FALSE_VALUE);
    return atomparsing::is_valid_false_atom(current_buf(), remaining_len(len));
  }
  WARN_UNUSED really_inline bool parse_root_null_atom(size_t len) {
    log_value("null");
    append_tape(0, internal::tape_type::NULL_VALUE);
    return atomparsing::is_valid_null_atom(current_buf(), remaining_len(len));
  }

  WARN_UNUSED really_inline bool parse_object() {
    if (parse_empty_object()) { return true; }
    return parse_object_fields(buf, doc_parser, next_structural_index, current_string_buf_loc);
  }

  WARN_UNUSED really_inline bool parse_empty_object() {
    // Special case (short circuit) empty object
    if (peek_char() == '}') {
      log_value("{}");
      advance_char();
      append_tape(doc_parser.current_loc+2, internal::tape_type::START_OBJECT);
      append_tape(doc_parser.current_loc-1, internal::tape_type::END_OBJECT);
      return true;
    }
    return false;
  }

  WARN_UNUSED really_inline bool parse_array() {
    if (parse_empty_array()) { return true; }
    return parse_array_values(buf, doc_parser, next_structural_index, current_string_buf_loc);
  }

  WARN_UNUSED really_inline bool parse_empty_array() {
    // Special case (short circuit) empty array
    if (peek_char() == ']') {
      log_value("[]");
      advance_char();
      append_tape(doc_parser.current_loc+2, internal::tape_type::START_ARRAY);
      append_tape(doc_parser.current_loc-1, internal::tape_type::END_ARRAY);
      return true;
    }
    return false;
  }

  WARN_UNUSED really_inline bool parse_object_fields() {
    log_start_value("object");
    // We will write the start array later
    const uint32_t start_loc = doc_parser.current_loc;
    doc_parser.current_loc++;

    // Parse the key/value pairs
    uint32_t count = 0;
    do {
      if (advance_char() != '"') { log_error("Key/value pair does not start with a string!"); return false; }
      log_value("key");
      if (!parse_string()) { return false; }
      if (advance_char() != ':') { log_error("No ':' in key/value pair!"); return false; }
      if (!parse_value()) { return false; }
      count++;
    } while (advance_char() == ',');

    // Append the end }, then write the { with count in it
    append_tape(start_loc, internal::tape_type::END_OBJECT);
    const uint32_t cntsat = count > 0xFFFFFF ? 0xFFFFFF : count;
    write_tape(start_loc, doc_parser.current_loc | (uint64_t(cntsat) << 32), internal::tape_type::START_OBJECT);

    // We know it's not a comma; make sure this is actually the end of an object!
    log_end_value("object");
    if (current_char() != '}') {
      log_error("object not terminated by }");
      return false;
    }
    return true;
  }

  WARN_UNUSED really_inline bool parse_array_values() {
    log_start_value("array");
    // We will write the start array later
    const uint32_t start_loc = doc_parser.current_loc;
    doc_parser.current_loc++;

    // Parse the values
    uint32_t count = 0;
    do {
      if (!parse_value()) { return false; }
      count++;
    } while (advance_char() == ',');

    // Append the end ], then write the [ with count in it
    append_tape(start_loc, internal::tape_type::END_ARRAY);
    const uint32_t cntsat = count > 0xFFFFFF ? 0xFFFFFF : count;
    write_tape(start_loc, doc_parser.current_loc | (uint64_t(cntsat) << 32), internal::tape_type::START_ARRAY);

    // We know it's not a comma; make sure this is actually the end of an array!
    log_end_value("array");
    if (current_char() != ']') {
      log_error("object not terminated by ]");
      return false;
    }
    return true;
  }

  WARN_UNUSED static bool parse_array_values(
    const uint8_t *buf,
    parser &doc_parser,
    const uint32_t *&next_structural_index,
    uint8_t *&current_string_buf_loc
  ) noexcept {
    structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc);
    return parser.parse_array_values();
  }

  WARN_UNUSED static bool parse_object_fields(
    const uint8_t *buf,
    parser &doc_parser,
    const uint32_t *&next_structural_index,
    uint8_t *&current_string_buf_loc
  ) noexcept {
    structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc);
    return parser.parse_object_fields();
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
    next_structural_index++;
    return current_char();
  }
  really_inline char current_char() {
    return *current_buf();
  }
  really_inline const uint8_t* current_buf() {
    return &buf[*(next_structural_index-1)];
  }
  really_inline size_t remaining_len(size_t len) {
    return buf + len - current_buf();
  }
  really_inline void write_tape(uint32_t loc, uint64_t val, internal::tape_type t) noexcept {
    doc_parser.doc.tape[loc] = val | ((uint64_t(char(t))) << 56);
  }
  really_inline void append_tape(uint64_t val, internal::tape_type t) noexcept {
    write_tape(doc_parser.current_loc, val, t);
    doc_parser.current_loc++;
  }

  really_inline void log_state(const char *state) {
    if (DEBUG) {
      printf("%*s%-*s %.10s -> %c\n", log_depth*2, "", LOG_TITLE_LEN - log_depth*2, state, current_buf(), peek_char());
    }
  }

  really_inline void log_value(const char *type) {
    if (DEBUG) {
      printf("%*s%-*s %.10s -> %c\n", log_depth*2, "", LOG_TITLE_LEN - log_depth*2, type, current_buf(), peek_char());
    }
  }

  static really_inline void log_start() {
    if (DEBUG) {
      log_depth = 0;
    }
  }

  really_inline void log_start_value(const char *type) {
    if (DEBUG) {
      printf("%*sstart %-*s %.10s -> %c\n", log_depth*2, "", LOG_TITLE_LEN - log_depth*2 - 6, type, current_buf(), peek_char());
      log_depth++;
    }
  }

  really_inline void log_end_value(const char *type) {
    if (DEBUG) {
      log_depth--;
      printf("%*send %-*s %.10s -> %c\n", log_depth*2, "", LOG_TITLE_LEN - log_depth*2 - 4, type, current_buf(), peek_char());
    }
  }

  really_inline void log_error(const char *error) {
    if (DEBUG) {
      printf("ERROR %s %.10s -> %c", error, current_buf(), peek_char());
    }
  }

}; // struct structural_parser

} // namespace {}

} // namespace stage2

/************
 * The JSON is parsed to a tape, see the accompanying tape.md file
 * for documentation.
 ***********/
WARN_UNUSED error_code implementation::stage2(const uint8_t *buf, size_t len, parser &doc_parser) const noexcept {
  const uint32_t *next_structural_index = &doc_parser.structural_indexes[0];
  uint8_t *current_string_buf_loc = &doc_parser.doc.string_buf[0];
  stage2::structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc);
  if (unlikely(!parser.parse_document(len))) {
    parser.log_state("error");
    doc_parser.valid = false;
    return doc_parser.error = parser.error();
  }

  if (*next_structural_index != len) {
    parser.log_error("Malformed document (extra JSON at the end)");
    doc_parser.valid = false;
    return doc_parser.error = TAPE_ERROR;
  }

  doc_parser.valid = true;
  return doc_parser.error = SUCCESS;
}

/************
 * The JSON is parsed to a tape, see the accompanying tape.md file
 * for documentation.
 ***********/
WARN_UNUSED error_code implementation::stage2(const uint8_t *buf, size_t len, parser &doc_parser, size_t &next_json) const noexcept {
  const uint32_t *next_structural_index = &doc_parser.structural_indexes[next_json];
  uint8_t *current_string_buf_loc = &doc_parser.doc.string_buf[0];
  stage2::structural_parser parser(buf, doc_parser, next_structural_index, current_string_buf_loc);
  if (unlikely(!parser.parse_document(len))) {
    parser.log_state("error");
    doc_parser.valid = false;
    return doc_parser.error = parser.error();
  }

  doc_parser.valid = true;
  // Update next_json to point at the next structural
  next_json = parser.next_structural_index - &doc_parser.structural_indexes[0];
  if (*next_structural_index != len) {
    parser.log_state("(and has more)");
    return doc_parser.error = SUCCESS_AND_HAS_MORE;
  }

  return doc_parser.error = SUCCESS;
}

WARN_UNUSED error_code implementation::parse(const uint8_t *buf, size_t len, parser &doc_parser) const noexcept {
  error_code code = stage1(buf, len, doc_parser, false);
  if (!code) {
    code = stage2(buf, len, doc_parser);
  }
  return code;
}
