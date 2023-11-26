#include "trumat.h"

static uint8_t TEXT[TEXT_SIZE];
static uint32_t HEAD;
static int I;
static struct text IN;

void init_memory() { HEAD = 0; }

static struct text text_slice(struct text, int, int);
static int parse_block_comment(struct text *);

static int parse_int(struct text *);

void dbg() {
  int start = 0;
  if (I > 10) {
    start = I - 10;
  }

  int end = TEXT_SIZE;
  if (TEXT_SIZE - I > 10) {
    end = I + 10;
  }

  for (int i = start; i < end; ++i) {
    if (TEXT[i] == '\n') {
      fputs("\\n", stdout);
    } else {
      putchar(TEXT[i]);
    }
  }

  putchar('\n');

  for (int i = start; i < I; ++i) {
    if (TEXT[i] == '\n') {
      putchar(' ');
    }
    putchar(' ');
  }
  fputs("^\n", stdout);
}

// It maps uppercase ASCII hex ABCDEF to lowercase hex abcdef. All other bytes
// are left unchanged.
const uint8_t hex_to_upper[256] = {
    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
    45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,
    75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
    90,  91,  92,  93,  94,  95,  96,  65,  66,  67,  68,  69,  70,  103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
    210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
    225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255};

int text_equal(struct text a, struct text b) {
  if (text_length(a) != text_length(b)) {
    return 0;
  }

  int i = 0;
  for (; text_index(a, i) >= 0 && text_index(a, i) == text_index(b, i); ++i) {
  }

  return text_length(a) == i;
}

void abcdef_to_upper(struct text t) {
  for (int i = 0; i + t.start < t.end; ++i) {
    TEXT[t.start + i] = hex_to_upper[TEXT[t.start + i]];
  }
}

void text_dbg(char *label, struct text t) {
  printf("%s: ", label);
  for (int i = t.start; i < t.end; ++i) {
    if (TEXT[i] == '\n') {
      fputs("\\n", stdout);
    } else {
      putchar(TEXT[i]);
    }
  }
  putchar('\n');
  for (int i = 0; label[i] != 0; ++i) {
    putchar(' ');
  }
  fputs("  ", stdout);
  for (int i = t.start; i < t.end; ++i) {
    if (TEXT[i] == '\n') {
      fputs("^^", stdout);
    } else {
      putchar('^');
    }
  }
  putchar('\n');
}

static struct text text_strip_end(struct text t) {
  int i = 0;
  int len = text_length(t);
  for (; text_index(t, len - i - 1) == ' '; ++i) {
  }

  return text_slice(t, 0, len - i);
}

static void append_char(char ch) {
  if (HEAD == TEXT_SIZE) {
    fputs("HEAD == TEXT_SIZE in append_char", stderr);
    exit(-1);
  }

  TEXT[HEAD] = ch;
  ++HEAD;
}

static struct text text_slice(struct text parent, int start, int end) {
  struct text result;
  int parent_size = text_length(parent);
  if (start > parent_size || end > parent_size || start > end) {
    fprintf(stderr, "bad slice: start %d, parent_size %d, end %d\n", start,
            parent_size, end);
    exit(-1);
  }

  result.start = parent.start + start;
  result.end = parent.start + end;
  return result;
}

static void copy_to_head(struct text t) {
  for (int i = 0;; ++i) {
    int result = text_index(t, i);
    if (result < 0) {
      break;
    }

    append_char(result);
  }
}

static struct text text_join(struct text left, struct text right) {
  struct text result;
  if (HEAD == left.end) {
    result.start = left.start;
  } else {
    result.start = HEAD;
  }

  if (left.end != HEAD) {
    copy_to_head(left);
  }

  copy_to_head(right);

  result.end = HEAD;
  return result;
}

static struct text text_append_ascii(struct text left, const char *right) {
  struct text result;
  if (left.end == HEAD) {
    result.start = left.start;
  } else {
    result.start = HEAD;
  }

  if (left.end != HEAD) {
    copy_to_head(left);
  }

  for (int i = 0; right[i] != '\0'; ++i) {
    append_char(right[i]);
  }

  result.end = HEAD;
  return result;
}

static struct text text_prepend_ascii_char(char left, struct text right) {
  struct text result;
  result.start = HEAD;
  append_char(left);

  copy_to_head(right);
  result.end = HEAD;
  return result;
}

static struct text text_append_ascii_char(struct text left, char right) {

  struct text result;

  if (left.end == HEAD) {
    result.start = left.start;
  } else {
    result.start = HEAD;
  }

  if (left.end != HEAD) {
    copy_to_head(left);
  }

  append_char(right);
  result.end = HEAD;
  return result;
}

static struct text text_from_ascii(char *ascii) {
  struct text result;
  result.start = HEAD;
  for (int i = 0; ascii[i] != '\0'; ++i) {
    append_char(ascii[i]);
  }

  result.end = HEAD;
  return result;
}

static struct text text_from_ascii_char(char ascii) {
  struct text result;
  result.start = HEAD;
  append_char(ascii);
  result.end = HEAD;
  return result;
}

static int parse_chunk(const char *chunk) {

  int start = I;

  int i = 0;
  for (; text_index(IN, I) == chunk[i] && chunk[i] != 0; ++I, ++i) {
  }

  if (chunk[i] == 0) {
    return 0;
  }

  I = start;
  return -1;
}

static int take_while_1(struct text *matching, const uint8_t match[256]) {

  int start = I;

  if (!(match[text_index(IN, I)])) {
    return -1;
  }
  ++I;

  for (; match[text_index(IN, I)]; ++I) {
  }

  *matching = text_slice(IN, start, I);
  return 0;
}

static struct text take_while(const uint8_t match[256]) {
  int start = I;

  for (; match[text_index(IN, I)]; ++I) {
  }

  return text_slice(IN, start, I);
}

static const uint8_t is_paragraph_char[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    1 /*Space*/, 1 /*!*/,   1 /*"*/,   1 /*#*/,
    1 /*$*/,     1 /*%*/,   1 /*&*/,   1 /*'*/,
    1 /*(*/,     1 /*)*/,   1 /***/,   1 /*+*/,
    1 /*,*/,     0 /*-*/,   1 /*.*/,   0 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   0 /*:*/,   1 /*;*/,
    0 /*<*/,     1 /*=*/,   0 /*>*/,   1 /*?*/,
    0 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   1 /*G*/,
    1 /*H*/,     1 /*I*/,   1 /*J*/,   1 /*K*/,
    1 /*L*/,     1 /*M*/,   1 /*N*/,   1 /*O*/,
    1 /*P*/,     1 /*Q*/,   1 /*R*/,   1 /*S*/,
    1 /*T*/,     1 /*U*/,   1 /*V*/,   1 /*W*/,
    1 /*X*/,     1 /*Y*/,   1 /*Z*/,   0 /*[*/,
    0 /*\*/,     0 /*]*/,   1 /*^*/,   0 /*_*/,
    0 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   1 /*g*/,
    1 /*h*/,     1 /*i*/,   1 /*j*/,   1 /*k*/,
    1 /*l*/,     1 /*m*/,   1 /*n*/,   1 /*o*/,
    1 /*p*/,     1 /*q*/,   1 /*r*/,   1 /*s*/,
    1 /*t*/,     1 /*u*/,   1 /*v*/,   1 /*w*/,
    1 /*x*/,     1 /*y*/,   1 /*z*/,   0 /*{*/,
    1 /*|*/,     0 /*}*/,   1 /*~*/,   0 /*DEL*/,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1};

static const uint8_t is_hex[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    0 /*Space*/, 0 /*!*/,   0 /*"*/,   0 /*#*/,
    0 /*$*/,     0 /*%*/,   0 /*&*/,   0 /*'*/,
    0 /*(*/,     0 /*)*/,   0 /***/,   0 /*+*/,
    0 /*,*/,     0 /*-*/,   0 /*.*/,   0 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   0 /*:*/,   0 /*;*/,
    0 /*<*/,     0 /*=*/,   0 /*>*/,   0 /*?*/,
    0 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   0 /*G*/,
    0 /*H*/,     0 /*I*/,   0 /*J*/,   0 /*K*/,
    0 /*L*/,     0 /*M*/,   0 /*N*/,   0 /*O*/,
    0 /*P*/,     0 /*Q*/,   0 /*R*/,   0 /*S*/,
    0 /*T*/,     0 /*U*/,   0 /*V*/,   0 /*W*/,
    0 /*X*/,     0 /*Y*/,   0 /*Z*/,   0 /*[*/,
    0 /*\*/,     0 /*]*/,   0 /*^*/,   0 /*_*/,
    0 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   0 /*g*/,
    0 /*h*/,     0 /*i*/,   0 /*j*/,   0 /*k*/,
    0 /*l*/,     0 /*m*/,   0 /*n*/,   0 /*o*/,
    0 /*p*/,     0 /*q*/,   0 /*r*/,   0 /*s*/,
    0 /*t*/,     0 /*u*/,   0 /*v*/,   0 /*w*/,
    0 /*x*/,     0 /*y*/,   0 /*z*/,   0 /*{*/,
    0 /*|*/,     0 /*}*/,   0 /*~*/,   0 /*DEL*/,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0,
    0,           0,         0,         0};

static const uint8_t is_digit[256] = {
    0 /*NUL*/, 0 /*SOH*/,   0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/, 0 /*ENQ*/,   0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,  0 /*TAB*/,   0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,  0 /*CR*/,    0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/, 0 /*DC1*/,   0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/, 0 /*NAK*/,   0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/, 0 /*EM*/,    0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,  0 /*GS*/,    0 /*RS*/,  0 /*RS*/,
    0 /*US*/,  0 /*Space*/, 0 /*!*/,   0 /*"*/,
    0 /*#*/,   0 /*$*/,     0 /*&*/,   0 /*'*/,
    0 /*(*/,   0 /*)*/,     0 /***/,   0 /*+*/,
    0 /*,*/,   0 /*-*/,     0 /*.*/,   0 /*forward slash*/,
    1 /*0*/,   1 /*1*/,     1 /*2*/,   1 /*3*/,
    1 /*4*/,   1 /*5*/,     1 /*6*/,   1 /*7*/,
    1 /*8*/,   1 /*9*/,     0 /*:*/,   0 /*;*/,
    0 /*<*/,   0 /*=*/,     0 /*>*/,   0 /*?*/,
    0 /*@*/,   0 /*A*/,     0 /*B*/,   0 /*C*/,
    0 /*D*/,   0 /*E*/,     0 /*F*/,   0 /*G*/,
    0 /*H*/,   0 /*I*/,     0 /*J*/,   0 /*K*/,
    0 /*L*/,   0 /*M*/,     0 /*N*/,   0 /*O*/,
    0 /*P*/,   0 /*Q*/,     0 /*R*/,   0 /*S*/,
    0 /*T*/,   0 /*U*/,     0 /*V*/,   0 /*W*/,
    0 /*X*/,   0 /*Y*/,     0 /*Z*/,   0 /*[*/,
    0 /*\*/,   0 /*]*/,     0 /*^*/,   0 /*_*/,
    0 /*`*/,   0 /*a*/,     0 /*b*/,   0 /*c*/,
    0 /*d*/,   0 /*e*/,     0 /*f*/,   0 /*g*/,
    0 /*h*/,   0 /*i*/,     0 /*j*/,   0 /*k*/,
    0 /*l*/,   0 /*m*/,     0 /*n*/,   0 /*o*/,
    0 /*p*/,   0 /*q*/,     0 /*r*/,   0 /*s*/,
    0 /*t*/,   0 /*u*/,     0 /*v*/,   0 /*w*/,
    0 /*x*/,   0 /*y*/,     0 /*z*/,   0 /*{*/,
    0 /*|*/,   0 /*}*/,     0 /*~*/,   0 /*DEL*/,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0,
    0,         0,           0,         0};

static const uint8_t is_triple_string_char[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 1 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    1 /*Space*/, 1 /*!*/,   0 /*"*/,   1 /*#*/,
    1 /*$*/,     1 /*%*/,   1 /*&*/,   1 /*'*/,
    1 /*(*/,     1 /*)*/,   1 /***/,   1 /*+*/,
    1 /*,*/,     1 /*-*/,   1 /*.*/,   1 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   1 /*:*/,   1 /*;*/,
    1 /*<*/,     1 /*=*/,   1 /*>*/,   1 /*?*/,
    1 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   1 /*G*/,
    1 /*H*/,     1 /*I*/,   1 /*J*/,   1 /*K*/,
    1 /*L*/,     1 /*M*/,   1 /*N*/,   1 /*O*/,
    1 /*P*/,     1 /*Q*/,   1 /*R*/,   1 /*S*/,
    1 /*T*/,     1 /*U*/,   1 /*V*/,   1 /*W*/,
    1 /*X*/,     1 /*Y*/,   1 /*Z*/,   1 /*[*/,
    0 /*\*/,     1 /*]*/,   1 /*^*/,   1 /*_*/,
    1 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   1 /*g*/,
    1 /*h*/,     1 /*i*/,   1 /*j*/,   1 /*k*/,
    1 /*l*/,     1 /*m*/,   1 /*n*/,   1 /*o*/,
    1 /*p*/,     1 /*q*/,   1 /*r*/,   1 /*s*/,
    1 /*t*/,     1 /*u*/,   1 /*v*/,   1 /*w*/,
    1 /*x*/,     1 /*y*/,   1 /*z*/,   1 /*{*/,
    1 /*|*/,     1 /*}*/,   1 /*~*/,   0 /*DEL*/,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1};

static const uint8_t is_line_comment_char[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    1 /*Space*/, 1 /*!*/,   1 /*"*/,   1 /*#*/,
    1 /*$*/,     1 /*%*/,   1 /*&*/,   1 /*'*/,
    1 /*(*/,     1 /*)*/,   1 /***/,   1 /*+*/,
    1 /*,*/,     1 /*-*/,   1 /*.*/,   1 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   1 /*:*/,   1 /*;*/,
    1 /*<*/,     1 /*=*/,   1 /*>*/,   1 /*?*/,
    1 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   1 /*G*/,
    1 /*H*/,     1 /*I*/,   1 /*J*/,   1 /*K*/,
    1 /*L*/,     1 /*M*/,   1 /*N*/,   1 /*O*/,
    1 /*P*/,     1 /*Q*/,   1 /*R*/,   1 /*S*/,
    1 /*T*/,     1 /*U*/,   1 /*V*/,   1 /*W*/,
    1 /*X*/,     1 /*Y*/,   1 /*Z*/,   1 /*[*/,
    1 /*\*/,     1 /*]*/,   1 /*^*/,   1 /*_*/,
    1 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   1 /*g*/,
    1 /*h*/,     1 /*i*/,   1 /*j*/,   1 /*k*/,
    1 /*l*/,     1 /*m*/,   1 /*n*/,   1 /*o*/,
    1 /*p*/,     1 /*q*/,   1 /*r*/,   1 /*s*/,
    1 /*t*/,     1 /*u*/,   1 /*v*/,   1 /*w*/,
    1 /*x*/,     1 /*y*/,   1 /*z*/,   1 /*{*/,
    1 /*|*/,     1 /*}*/,   1 /*~*/,   0 /*DEL*/,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1};

static const uint8_t is_normal_string_char[256] = {
    0 /*NUL*/,   0 /*SOH*/, 0 /*STX*/, 0 /*ETX*/,
    0 /*EOT*/,   0 /*ENQ*/, 0 /*ACK*/, 0 /*BEL*/,
    0 /*BS*/,    0 /*TAB*/, 0 /*LF*/,  0 /*VT*/,
    0 /*FF*/,    0 /*CR*/,  0 /*SO*/,  0 /*SI*/,
    0 /*DLE*/,   0 /*DC1*/, 0 /*DC2*/, 0 /*DC3*/,
    0 /*DC4*/,   0 /*NAK*/, 0 /*SYN*/, 0 /*ETB*/,
    0 /*CAN*/,   0 /*EM*/,  0 /*SUB*/, 0 /*ESC*/,
    0 /*FS*/,    0 /*GS*/,  0 /*RS*/,  0 /*US*/,
    1 /*Space*/, 1 /*!*/,   0 /*"*/,   1 /*#*/,
    1 /*$*/,     1 /*%*/,   1 /*&*/,   1 /*'*/,
    1 /*(*/,     1 /*)*/,   1 /***/,   1 /*+*/,
    1 /*,*/,     1 /*-*/,   1 /*.*/,   1 /*forward slash*/,
    1 /*0*/,     1 /*1*/,   1 /*2*/,   1 /*3*/,
    1 /*4*/,     1 /*5*/,   1 /*6*/,   1 /*7*/,
    1 /*8*/,     1 /*9*/,   1 /*:*/,   1 /*;*/,
    1 /*<*/,     1 /*=*/,   1 /*>*/,   1 /*?*/,
    1 /*@*/,     1 /*A*/,   1 /*B*/,   1 /*C*/,
    1 /*D*/,     1 /*E*/,   1 /*F*/,   1 /*G*/,
    1 /*H*/,     1 /*I*/,   1 /*J*/,   1 /*K*/,
    1 /*L*/,     1 /*M*/,   1 /*N*/,   1 /*O*/,
    1 /*P*/,     1 /*Q*/,   1 /*R*/,   1 /*S*/,
    1 /*T*/,     1 /*U*/,   1 /*V*/,   1 /*W*/,
    1 /*X*/,     1 /*Y*/,   1 /*Z*/,   1 /*[*/,
    0 /*\*/,     1 /*]*/,   1 /*^*/,   1 /*_*/,
    1 /*`*/,     1 /*a*/,   1 /*b*/,   1 /*c*/,
    1 /*d*/,     1 /*e*/,   1 /*f*/,   1 /*g*/,
    1 /*h*/,     1 /*i*/,   1 /*j*/,   1 /*k*/,
    1 /*l*/,     1 /*m*/,   1 /*n*/,   1 /*o*/,
    1 /*p*/,     1 /*q*/,   1 /*r*/,   1 /*s*/,
    1 /*t*/,     1 /*u*/,   1 /*v*/,   1 /*w*/,
    1 /*x*/,     1 /*y*/,   1 /*z*/,   1 /*{*/,
    1 /*|*/,     1 /*}*/,   1 /*~*/,   0 /*DEL*/,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1,
    1,           1,         1,         1};

static int parse_char(char ch) {
  int got = text_index(IN, I);
  if (got < 0) {
    return -1;
  }

  if (got != ch) {
    return -1;
  }

  ++I;
  return 0;
}

static int parse_positive_hex_int(struct text *expression) {
  int start = I;

  int result = parse_chunk("0x");
  if (result) {
    I = start;
    return result;
  }

  *expression = text_from_ascii("0x");

  struct text after_0x;
  result = take_while_1(&after_0x, is_hex);
  if (result) {
    I = start;
    return result;
  }

  abcdef_to_upper(after_0x);

  *expression = text_join(*expression, after_0x);

  return 0;
}

static int parse_positive_int(struct text *expression) {
  int result = parse_positive_hex_int(expression);
  if (result == 0) {
    return result;
  }

  return take_while_1(expression, is_digit);
}

static int parse_negative_int(struct text *expression) {
  int start = I;

  int result = parse_char('-');
  if (result) {
    I = start;
    return result;
  }

  struct text exponent;
  result = parse_positive_int(&exponent);
  if (result) {
    I = start;
    return result;
  }

  *expression = text_prepend_ascii_char('-', exponent);
  return 0;
}

static int parse_int(struct text *expression) {

  int result = parse_positive_int(expression);
  if (result == 0) {
    return 0;
  }

  return parse_negative_int(expression);
}

static int parse_simple_float(struct text *expression) {

  int start = I;

  int result = take_while_1(expression, is_digit);
  if (result) {
    I = start;
    return result;
  }

  result = parse_char('.');
  if (result) {
    I = start;
    return result;
  }
  *expression = text_append_ascii_char(*expression, '.');

  struct text after_dot;
  result = take_while_1(&after_dot, is_digit);
  if (result) {
    I = start;
    return result;
  }

  *expression = text_join(*expression, after_dot);
  return 0;
}

static int parse_float_exponent(struct text *expression) {
  int start = I;

  int result = parse_char('e');
  if (result) {
    I = start;
    return result;
  }

  struct text exponent;
  result = parse_int(&exponent);
  if (result) {
    I = start;
    return result;
  }

  *expression = text_prepend_ascii_char('e', exponent);
  return 0;
}

static int parse_dot_exponent_float(struct text *expression) {
  int start = I;

  int result = parse_simple_float(expression);
  if (result) {
    I = start;
    return result;
  }

  struct text exponent;
  result = parse_float_exponent(&exponent);
  if (result) {
    I = start;
    return result;
  }
  *expression = text_join(*expression, exponent);
  return 0;
}

static int parse_non_dot_exponent_float(struct text *expression) {

  int start = I;

  int result = take_while_1(expression, is_digit);
  if (result) {
    I = start;
    return result;
  }

  struct text exponent;
  result = parse_float_exponent(&exponent);
  if (result) {
    I = start;
    return result;
  }

  *expression = text_append_ascii(*expression, ".0");

  *expression = text_join(*expression, exponent);
  return 0;
}

static int parse_positive_float(struct text *expression) {
  int start = I;

  int result = parse_dot_exponent_float(expression);
  if (result == 0) {
    return 0;
  }

  result = parse_simple_float(expression);
  if (result == 0) {
    return 0;
  }

  result = parse_non_dot_exponent_float(expression);
  if (result == 0) {
    return 0;
  }

  I = start;
  return -1;
}

static int parse_negative_float(struct text *expression) {
  int start = I;

  int result = parse_char('-');
  if (result) {
    I = start;
    return result;
  }

  struct text positive;
  result = parse_positive_float(&positive);
  if (result) {
    I = start;
    return result;
  }

  *expression = text_prepend_ascii_char('-', positive);
  return 0;
}

static int parse_float(struct text *expression) {
  int result = parse_positive_float(expression);
  if (result == 0) {
    return 0;
  }

  return parse_negative_float(expression);
}

static int parse_unicode_hex(struct text *formatted) {
  int start = I;

  int result = parse_chunk("\\u{");
  if (result) {
    I = start;
    return result;
  }

  struct text unicode;
  result = take_while_1(&unicode, is_hex);
  if (result) {
    I = start;
    return result;
  }

  abcdef_to_upper(unicode);

  result = parse_char('}');
  if (result) {
    I = start;
    return result;
  }

  *formatted = text_from_ascii("\\u{");
  *formatted = text_join(*formatted, unicode);
  *formatted = text_append_ascii_char(*formatted, '}');

  return 0;
}

static int parse_double_quote_in_triple_string(struct text *expression) {
  int start = I;

  int result = parse_char('"');
  if (result) {
    I = start;
    return result;
  }
  *expression = text_from_ascii_char('"');

  result = parse_chunk("\"\"");
  if (result == 0) {
    I = start;
    return -1;
  }

  return 0;
}

static int parse_triple_string_piece(struct text *expression) {
  int result = take_while_1(expression, is_triple_string_char);
  if (result == 0) {
    return result;
  }

  result = parse_chunk("\\n");
  if (result == 0) {
    *expression = text_from_ascii_char('\n');
    return 0;
  }

  result = parse_chunk("\\\"");
  if (result == 0) {
    *expression = text_from_ascii("\\\"");
    return 0;
  }

  result = parse_double_quote_in_triple_string(expression);
  if (result == 0) {
    return 0;
  }

  result = parse_chunk("\\\\");
  if (result == 0) {
    *expression = text_from_ascii("\\\\");
    return 0;
  }

  result = parse_chunk("\\t");
  if (result == 0) {
    *expression = text_from_ascii("\\t");
    return 0;
  }

  result = parse_chunk("\\r");
  if (result == 0) {
    *expression = text_from_ascii("\\u{000D}");
    return 0;
  }

  return parse_unicode_hex(expression);
}

static int parse_simple_string_piece(struct text *expression) {
  int result = take_while_1(expression, is_normal_string_char);
  if (result == 0) {
    return result;
  }

  result = parse_chunk("\\\"");
  if (result == 0) {
    *expression = text_from_ascii("\\\"");
    return 0;
  }

  result = parse_chunk("\\\\");
  if (result == 0) {
    *expression = text_from_ascii("\\\\");
    return 0;
  }

  result = parse_chunk("\\n");
  if (result == 0) {
    *expression = text_from_ascii("\\n");
    return 0;
  }

  result = parse_chunk("\\t");
  if (result == 0) {
    *expression = text_from_ascii("\\t");
    return 0;
  }

  result = parse_chunk("\\r");
  if (result == 0) {
    *expression = text_from_ascii("\\u{000D}");
    return 0;
  }

  return parse_unicode_hex(expression);
}

static int parse_triple_string(struct text *expression) {
  int start = I;

  int result = parse_chunk("\"\"\"");
  if (result) {
    I = start;
    return result;
  }
  *expression = text_from_ascii("\"\"\"");

  struct text contents;
  while (parse_triple_string_piece(&contents) == 0) {
    *expression = text_join(*expression, contents);
  }

  result = parse_chunk("\"\"\"");
  if (result) {
    I = start;
    return result;
  }

  *expression = text_append_ascii(*expression, "\"\"\"");
  return 0;
}

static int parse_simple_string(struct text *expression) {
  int start = I;

  int result = parse_char('"');
  if (result) {
    I = start;
    return result;
  }
  *expression = text_from_ascii_char('"');

  struct text contents;
  while (parse_simple_string_piece(&contents) == 0) {
    *expression = text_join(*expression, contents);
  }

  result = parse_char('"');
  if (result) {
    I = start;
    return result;
  }
  *expression = text_append_ascii_char(*expression, '"');
  return 0;
}

static int parse_expression(struct text *expression) {

  int result = parse_float(expression);
  if (result == 0) {
    return 0;
  }

  result = parse_int(expression);
  if (result == 0) {
    return 0;
  }

  result = parse_triple_string(expression);
  if (result == 0) {
    return 0;
  }

  return parse_simple_string(expression);
}

static void parse_spaces_and_newlines() {
  for (; text_index(IN, I) == ' ' || text_index(IN, I) == '\n'; ++I) {
  }
}

static int parse_line_comment(struct text *comment) {
  int start = I;

  int result = parse_chunk("--");
  if (result) {
    I = start;
    return result;
  }

  *comment = text_from_ascii("--");

  struct text contents = take_while(is_line_comment_char);
  contents = text_strip_end(contents);

  *comment = text_join(*comment, contents);

  return 0;
}

int parse_empty_block_comment(struct text *comment) {
  int result = parse_chunk("{--}");
  if (result) {
    return result;
  }

  *comment = text_from_ascii("{--}");
  return 0;
}

static int parse_block_comment_space(struct text *comment) {
  int start = I;
  int result = parse_chunk("{-");
  if (result) {
    return result;
  }
  if (parse_char('\n') && parse_char(' ')) {
    I = start;
    return -1;
  }

  while (!parse_char('\n') || !parse_char(' ')) {
  }

  result = parse_chunk("-}");
  if (result) {
    I = start;
    return result;
  }

  *comment = text_from_ascii("{- -}");
  return 0;
}

static int parse_paragraph_line_break() {
  int result = parse_char('\n');
  if (result) {
    return result;
  }

  while (parse_char(' ') == 0) {
  }

  return 0;
}

static struct text text_strip(struct text untrimmed) {
  struct text trimmed = untrimmed;
  for (; (TEXT[trimmed.start] == ' ' || TEXT[trimmed.start] == '\n') &&
         trimmed.start < trimmed.end;
       ++trimmed.start) {
  }
  for (; (TEXT[trimmed.end - 1] == ' ' || TEXT[trimmed.end - 1] == '\n') &&
         trimmed.end > trimmed.start;
       --trimmed.end) {
  }

  return trimmed;
}

static int parse_paragraph_line(struct text *line) {
  int start = I;
  int result = take_while_1(line, is_paragraph_char);
  if (result) {
    I = start;
    return result;
  }
  *line = text_strip(*line);
  if (text_length(*line) == 0) {
    I = start;
    return -1;
  }
  return 0;
}

static int parse_paragraph(struct text *item) {
  int start = I;
  int result = parse_paragraph_line(item);
  if (result) {
    I = start;
    return result;
  }

  while (1) {
    start = I;
    result = parse_paragraph_line_break();
    if (result) {
      I = start;
      return 0;
    }
    struct text line;
    result = parse_paragraph_line(&line);
    if (result) {
      I = start;
      return 0;
    }
    *item = text_append_ascii(*item, "\n      ");
    *item = text_join(*item, line);
  }
  return 0;
}

static int parse_block_comment_item(struct text *item) {
  int result = parse_paragraph(item);
  if (result == 0) {
    return 0;
  }
  return parse_block_comment(item);
}

static int parse_empty_line() {
  int start = I;
  while (parse_char(' ') == 0) {
  }
  if (parse_char('\n')) {
    I = start;
    return -1;
  }
  return 0;
}

static struct text parse_block_comment_contents() {
  struct text items[200];
  int num_items = 0;
  while (num_items < 200) {
    struct text item = {0, 0};
    int result = parse_block_comment_item(&item);
    if (result == 0) {
      items[num_items] = item;
      ++num_items;
    }
    if (result) {
      result = parse_empty_line();
    }
    if (result) {
      break;
    }
  }

  struct text contents = {0, 0};
  for (int i = 0; i < num_items - 1; ++i) {
    contents = text_join(contents, items[i]);
    contents = text_append_ascii(contents, "\n      ");
  }

  if (num_items > 0) {
    contents = text_join(contents, items[num_items - 1]);
  }

  return contents;
}

static int parse_hanging_block_comment(struct text *comment) {
  int start = I;
  int result = parse_chunk("{-");
  if (result) {
    I = start;
    return result;
  }
  while (parse_char(' ') == 0) {
  }
  if (parse_char('\n')) {
    I = start;
    return -1;
  }
  *comment = text_from_ascii("{-\n       ");
  struct text contents = parse_block_comment_contents();
  while (!parse_char(' ') || !parse_char('\n')) {
  }
  *comment = text_join(*comment, contents);
  result = parse_chunk("-}");
  if (result) {
    I = start;
    return result;
  }
  *comment = text_append_ascii(*comment, "\n    -}");
  return 0;
}

static int text_is_multiline(struct text t) {
  for (int i = t.start; i < t.end; ++i) {
    if (TEXT[i] == '\n') {
      return 1;
    }
  }

  return 0;
}

static int parse_single_line_block_comment(struct text *comment) {
  int start = I;
  int result = parse_chunk("{-");
  if (result) {
    I = start;
    return result;
  }

  *comment = text_from_ascii("{- ");
  while (!parse_char(' ')) {
  }
  struct text contents = parse_block_comment_contents();
  if (text_is_multiline(contents)) {
    I = start;
    return -1;
  }
  *comment = text_join(*comment, contents);
  while (!parse_char(' ')) {
  }
  result = parse_chunk("-}");
  if (result) {
    I = start;
    return result;
  }
  *comment = text_append_ascii(*comment, " -}");
  return 0;
}

static int parse_block_comment(struct text *comment) {
  int result = parse_empty_block_comment(comment);
  if (result == 0) {
    return 0;
  }

  result = parse_block_comment_space(comment);
  if (result == 0) {
    return 0;
  }

  result = parse_hanging_block_comment(comment);
  if (result == 0) {
    return 0;
  }

  return parse_single_line_block_comment(comment);
}

static int parse_one_comment(struct text *comment) {
  int result = parse_line_comment(comment);
  if (result == 0) {
    return 0;
  }

  return parse_block_comment(comment);
}

static int parse_some_comments(struct text *comments) {
  parse_spaces_and_newlines();
  int result = parse_one_comment(comments);
  if (result) {
    return 0;
  }
  struct text line_comment;
  while (1) {
    parse_spaces_and_newlines();
    result = parse_one_comment(&line_comment);
    if (result) {
      break;
    }
    *comments = text_append_ascii(*comments, "\n    ");
    *comments = text_join(*comments, line_comment);
  }

  return 0;
}

int format(const struct text in, struct text *out) {
  IN = in;
  I = 0;
  int result = parse_chunk("module X exposing (x)\n\n\nx =\n");
  if (result < 0) {
    return result;
  }

  struct text commentBefore = {.start = 0, .end = 0};
  result = parse_some_comments(&commentBefore);
  if (result) {
    return result;
  }

  struct text expression;
  result = parse_expression(&expression);
  if (result < 0) {
    return result;
  }

  result = parse_chunk("\n\0");
  if (result < 0) {
    return result;
  }

  *out = text_from_ascii("module X exposing (x)\n\n\nx =\n    ");
  *out = text_join(*out, commentBefore);
  if (text_length(commentBefore) > 0) {
    *out = text_append_ascii(*out, "\n    ");
  }
  *out = text_join(*out, expression);
  *out = text_append_ascii_char(*out, '\n');

  return 0;
}

void make_sub_path(const char *parent, const char *child, char *result) {
  int i = 0;
  for (; parent[i] != 0; ++i) {
    result[i] = parent[i];
  }
  result[i] = '/';
  ++i;

  int j = 0;
  for (; child[j] != 0; ++j) {
    result[i + j] = child[j];
  }
  i += j;
  result[i] = 0;
}

static int string_length(const char *path) {
  int i = 0;
  for (; path[i] != 0; ++i) {
  }
  return i;
}

int is_dot_path(const char *path) {
  int length = string_length(path);
  return (length == 1 && path[0] == '.') ||
         (length == 2 && path[0] == '.' && path[1] == '.');
}

int is_elm_path(const char *path) {
  int length = string_length(path);
  return length >= 4 && path[length - 1] == 'm' && path[length - 2] == 'l' &&
         path[length - 3] == 'e' && path[length - 4] == '.';
}

int string_equal(const char *a, const char *b) {
  for (; *a == *b && *a != 0 && *b != 0; ++a, ++b) {
  }

  return *a == 0 && *b == 0;
}

int text_length(struct text t) { return t.end - t.start; }

int text_from_file(FILE *file, struct text *t) {
  t->start = HEAD;
  size_t size = fread(TEXT + t->start, 1, TEXT_SIZE - t->start, file);
  HEAD += size;
  t->end = HEAD;
  return 0;
}

void text_to_file(FILE *f, struct text t) {
  fwrite(TEXT + t.start, 1, t.end - t.start, f);
}

int text_index(struct text t, int index) {
  if (index < 0 || t.start + index >= t.end || t.start + index >= TEXT_SIZE) {
    return -1;
  }

  return TEXT[t.start + index];
}
