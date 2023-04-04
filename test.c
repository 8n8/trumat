struct Case {
    char* description;
    char* in;
    char* out;
}

struct Case cases[] = {
    { .description = "hello world formatted",
      .in =
        "module X exposing (x)\n"
        "\n"
        "\n"
        "x =\n"
        "    0\n",
      .out =
        "module X exposing (x)\n"
        "\n"
        "\n"
        "x =\n"
        "    0\n",
    }
};
