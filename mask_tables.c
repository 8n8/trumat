enum state {
  state_in_normal_string,
  state_in_triple_string,
  state_after_backslash_triple_string,
  state_after_double_quote_triple_string,
  state_after_2nd_double_quote_triple_string,
  state_after_backslash_normal_string,
  state_after_open_curly_outside,
  state_after_open_curly_hyphen_outside,
  state_after_block_hyphen,
  state_after_doc_hyphen,
  state_in_block_comment,
  state_in_doc_comment,
  state_in_line_comment,
  state_outside,
};

const enum state all_states[14] = {
  state_in_normal_string,
  state_in_triple_string,
  state_after_backslash_triple_string,
  state_after_double_quote_triple_string,
  state_after_2nd_double_quote_triple_string,
  state_after_backslash_normal_string,
  state_after_open_curly_outside,
  state_after_open_curly_hyphen_outside,
  state_after_block_hyphen,
  state_after_doc_hyphen,
  state_in_block_comment,
  state_in_doc_comment,
  state_in_line_comment,
  state_outside,
};

enum nesting {
  nesting_1,
  nesting_2,
  nesting_3,
  nesting_4,
  nesting_5,
}

const enum nesting all_nesting[5] = {
  nesting_1,
  nesting_2,
  nesting_3,
  nesting_4,
  nesting_5,
};

enum character {
  char_hyphen,
  char_backslash,
  char_double_quote,
  char_open_curly,
  char_close_curly,
  char_newline,
  char_other,
};

const enum character all_characters[6] = {
  char_hyphen,
  char_backslash,
  char_double_quote,
  char_open_curly,
  char_close_curly,
  char_newline,
  char_other,
};

int calculate_row(enum character ch, enum nesting n, enum state s) {
  switch (s) {
    case state_in_normal_string:
      switch (ch) {
        case char_hyphen:
          return state_in_normal_string;
        case char_backslash:
          return state_after_backslash_normal_string;
        case char_double_quote:
          return state_outside;
        case char_open_curly:
          return state_in_normal_string;
        case char_close_curly:
          return state_in_normal_string;
        case char_newline:
          return -1;
      }
    case state_in_triple_string:
      switch (ch) {
        case char_hyphen:
          return state_in_triple_string;
        case char_backslash:
          return state_after_backslash_triple_string;
        case char_double_quote:
          return state_after_double_quote_triple_string;
        case char_open_curly:
          return state_in_triple_string;
        case char_close_curly:
          return state_in_triple_string;
        case char_newline:
          return -1;
      }
    case state_after_backslash_triple_string:
      switch (ch) {
        case char_hyphen:
          return state_in_triple_string;
        case char_backslash:
          return state_in_triple_string;
        case char_double_quote:
          return state_in_triple_string;
        case char_open_curly:
          return state_in_triple_string;
        case char_close_curly:
          return state_in_triple_string;
        case char_newline:
          return state_in_triple_string;
      }
    case state_after_double_quote_triple_string:
      switch (ch) {
        case char_hyphen:
          return state_in_triple_string;
        case char_backslash:
          return state_after_backslash_triple_string;
        case char_double_quote:
          return state_after_2nd_double_quote_triple_string;
        case char_open_curly:
          return state_in_triple_string;
        case char_close_curly:
          return state_in_triple_string;
        case char_newline:
          return state_in_triple_string;
      }
    case state_after_2nd_double_quote_triple_string:
      switch (ch) {
        case char_hyphen:
          return state_in_triple_string;
        case char_backslash:
          return state_after_backslash_triple_string;
        case char_double_quote:
          return state_outside;
        case char_open_curly:
          return state_in_triple_string;
        case char_close_curly:
          return state_in_triple_string;
        case char_newline:
          return state_in_triple_string;
      }
    case state_after_backslash_normal_string:
      switch (ch) {
        case char_hyphen:
          return state_in_normal_string;
        case char_backslash:
          return state_in_normal_string;
        case char_double_quote:
          return state_in_normal_string;
        case char_open_curly:
          return state_in_normal_string;
        case char_close_curly:
          return state_in_normal_string;
        case char_newline:
          return -1;
      }
    case state_after_open_curly_outside:
      switch (ch) {
        case char_hyphen:
          return state_after_open_curly_hyphen_outside;
        case char_backslash:
          return state_outside;
        case char_double_quote:
          return state_in_normal_string;
        case char_open_curly:
          return state_after_open_curly_outside;
        case char_close_curly:
          return state_outside;
        case char_newline:
          return state_outside;
        case char_other:
          return state_outside;
      }
    case state_after_open_curly_hyphen_outside:
      switch (ch) {
        case char_hyphen:
          return state_after_block_hyphen;
        case char_backslash:
          return state_outside;
        case char_double_quote:
          return state_in_normal_string;
        case char_open_curly:
          return state_after_open_curly_outside;
        case char_close_curly:
          return state_outside;
        case char_newline:
          return state_outside;
        case char_other:
          return state_outside;
      }
    }
}

int main(int argc, char* argv[]) {
  for (int i = 0; i < 6; ++i) {
    enum character ch = all_characters[i];
    for (int j = 0; j < 5; ++j) {
      enum nesting n = all_nesting[j];
      for (int k = 0; k < 11; ++k) {
        enum state s = all_states[k];
        int row = calculate_row(ch, n, s);
        printf("%d\n", row);
      }
    }
  }
}
