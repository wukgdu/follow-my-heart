#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// parse_string's max length
#define max_len 200
// TOP grammar
// int blank stands for layers of ast
void TOP(int);
void assign_expr(int);
void expr(int);
void term(int);
void negfactor(int);
void factor(int);
void while_stmt(int);
void some_stmt(int);
// get a token
void lex();
// help for lex
void getChar();
void addChar();
// peek next token
void look_next();
// remove space in parse_string
void remove_blank();
// help to print space for layers of grammar
void print_blank(int);
// print token from lex
void printlex(int);

// default string to parse
char parse_string[max_len] = "while((a>b)&c<d|((e<f))) { a=bc*a+c+-4; while (c<d) {b=2* -c +3; a = (b) * -b;}} a = a + 1;";
// now index of parse_string
int parse_idx = -1;
char now_char = '\0';
// char type for regex
enum char_type { LETTER, DIGIT, UNKOWN, MINUS, PLUS, MULT, DIVI, LEFT_PAR, RIGHT_PAR, END, SCOLON, EQ, LEFT_BRACE, RIGHT_BRACE, AND, OR, MORE, LESS };
// lex's token type
enum token_type { ID_CODE, PLUS_CODE, MINUS_CODE, MULT_CODE, DIVI_CODE, LEFT_PAREN_CODE, RIGHT_PAREN_CODE, END_CODE, NUM_CODE, SCOLON_CODE, EQ_CODE, LEFT_BRACE_CODE, RIGHT_BRACE_CODE, UNKOWN_CODE, AND_CODE, OR_CODE, MORE_CODE, LESS_CODE };
int char_class = -1;
int next_token = -1;
// lex's buffer for token
char buffer[10] = "\0";
int buf_idx = 0;

int main(void) {
    parse_idx = -1;
    fgets(parse_string, max_len, stdin);
    remove_blank();
    TOP(0);
    return 0;
}

// str: error cause
void error(char *str) {
    printf("parse error: %s\n", str);
    exit(0);
}

void TOP2(int blank) {
    print_blank(blank);
    printf("TOP2\n");
    look_next();
    if (next_token == ID_CODE) {
        lex();
        look_next();
        if (strcmp(buffer, "while") == 0 && next_token == LEFT_PAREN_CODE) {
            while_stmt(blank+1);
        } else {
            assign_expr(blank+1);
        }
        TOP2(blank+1);
    } else if (next_token != END_CODE) {
        error("expect id or eof");
    } else {
        print_blank(blank+1);
        printf("nil\n");
    }
}

// TOP -> [assign_expr || while_stmt]+
// see doc for more
void TOP(int blank) {
    printf("TOP\n");
    look_next();
    if (next_token != ID_CODE) {
        error("expect id");
    }
    lex();
    look_next();
    if (strcmp(buffer, "while") == 0 && next_token == LEFT_PAREN_CODE) {
        while_stmt(blank+1);
    } else {
        assign_expr(blank+1);
    }
    TOP2(blank+1);
}

void some_stmt2(int blank) {
    print_blank(blank);
    printf("some_stmt2\n");
    look_next();
    if (next_token == ID_CODE) {
        lex();
        if (strcmp(buffer, "while") == 0) {
            while_stmt(blank+1);
        } else {
            assign_expr(blank+1);
        }
        some_stmt2(blank+1);
    } else if (next_token != RIGHT_BRACE_CODE) {
        error("expect id or }");
    } else {
        print_blank(blank+1);
        printf("nil\n");
    }
}

void some_stmt(int blank) {
    print_blank(blank);
    printf("some_stmt\n");
    look_next();
    if (next_token != ID_CODE) {
        error("expect id");
    }
    lex();
    look_next();
    if (strcmp(buffer, "while") == 0 && next_token == LEFT_PAREN_CODE) {
        while_stmt(blank+1);
    } else {
        assign_expr(blank+1);
    }
    some_stmt2(blank+1);
}

void bool_clause(int blank) {
    print_blank(blank);
    printf("bool_clause\n");
    look_next();
    if (next_token != ID_CODE) {
        error("expect id");
    }
    lex(); printlex(blank+1);
    look_next();
    if (next_token != MORE_CODE && next_token != LESS_CODE) {
        error("expect > or <");
    }
    lex(); printlex(blank+1);
    look_next();
    if (next_token != ID_CODE) {
        error("expect id");
    }
    lex(); printlex(blank+1);
}

void bool_stmt(int blank);
void bool_stmt2(int blank) {
    print_blank(blank);
    printf("bool_stmt2\n");
    look_next();
    if (next_token == AND_CODE || next_token == OR_CODE) {
        lex(); printlex(blank+1);
        bool_stmt(blank+1);
    } else if (next_token != RIGHT_PAREN_CODE) {
        error("expect & | or )");
    } else {
        print_blank(blank+1);
        printf("nil\n");
    }
}

void bool_stmt(int blank) {
    print_blank(blank);
    printf("bool_stmt\n");
    look_next();
    if (next_token == LEFT_PAREN_CODE) {
        lex(); printlex(blank+1);
        bool_stmt(blank+1);
        look_next();
        if (next_token != RIGHT_PAREN_CODE) {
            error("expect )");
        }
        lex(); printlex(blank+1);
    } else if (next_token == ID_CODE) {
        bool_clause(blank+1);
    } else {
        error("expect ( or id");
    }
    bool_stmt2(blank+1);
}

void while_stmt(int blank) {
    print_blank(blank);
    printf("while_stmt\n");
    printlex(blank+1);
    look_next();
    if (next_token != LEFT_PAREN_CODE) {
        error("expect (");
    }
    lex(); printlex(blank+1);
    bool_stmt(blank+1);
    look_next();
    if (next_token != RIGHT_PAREN_CODE) {
        error("expect )");
    }
    lex(); printlex(blank+1);
    look_next();
    if (next_token != LEFT_BRACE_CODE) {
        error("expect {");
    }
    lex(); printlex(blank+1);
    some_stmt(blank+1);
    look_next();
    if (next_token != RIGHT_BRACE_CODE) {
        error("expect }");
    }
    lex(); printlex(blank+1);
}

void assign_expr(int blank) {
    print_blank(blank);
    printf("assign_expr\n");
    printlex(blank+1);
    look_next();
    if (next_token != EQ_CODE) {
        error("expect =");
    }
    lex(); printlex(blank+1);
    expr(blank+1);
    look_next();
    if (next_token != SCOLON_CODE) {
        error("expect ;");
    } else {
        lex(); printlex(blank+1);
    }
}

void expr2(int blank) {
    print_blank(blank);
    printf("expr2\n");
    look_next();
    if (next_token == PLUS_CODE || next_token == MINUS_CODE) {
        lex(); printlex(blank+1);
        term(blank+1);
        expr2(blank+1);
    } else if (next_token != RIGHT_PAREN_CODE && next_token != SCOLON_CODE) {
        error("expect + - ) or ;");
    } else {
        print_blank(blank+1);
        printf("nil\n");
    }
}

void expr(int blank) {
    print_blank(blank);
    printf("expr\n");
    look_next();
    if (next_token != NUM_CODE && next_token != ID_CODE 
        && next_token != MINUS_CODE && next_token != LEFT_PAREN_CODE) {
        error("expect num id - or (");
    }
    term(blank+1);
    expr2(blank+1);
}

void term2(int blank) {
    print_blank(blank);
    printf("term2\n");
    look_next();
    if (next_token == MULT_CODE || next_token == DIVI_CODE) {
        lex(); printlex(blank+1);
        negfactor(blank+1);
        term2(blank+1);
    } else if (next_token != RIGHT_PAREN_CODE && next_token != SCOLON_CODE
            && next_token != MINUS_CODE && next_token != PLUS_CODE) {
        error("expect * / ) ; - or +");
    } else {
        print_blank(blank+1);
        printf("nil\n");
    }
}

void term(int blank) {
    print_blank(blank);
    printf("term\n");
    look_next();
    if (next_token != NUM_CODE && next_token != ID_CODE 
        && next_token != MINUS_CODE && next_token != LEFT_PAREN_CODE) {
        error("expect num id - or (");
    }
    negfactor(blank+1);
    term2(blank+1);
}

void negfactor(int blank) {
    print_blank(blank);
    printf("negfactor\n");
    look_next();
    if (next_token == MINUS_CODE) {
        lex(); printlex(blank+1);
        factor(blank+1);
    } else if (next_token == NUM_CODE || next_token == ID_CODE
            || next_token == LEFT_PAREN_CODE) {
        factor(blank+1);
    } else {
        error("expect - num id or (");
    }
}

void factor(int blank) {
    print_blank(blank);
    printf("factor\n");
    look_next();
    if (next_token == ID_CODE) {
        lex(); printlex(blank+1);
    } else if (next_token == NUM_CODE) {
        lex(); printlex(blank+1);
    } else if (next_token == LEFT_PAREN_CODE) {
        lex(); printlex(blank+1);
        expr(blank+1);
        look_next();
        if (next_token == RIGHT_PAREN_CODE) {
            lex(); printlex(blank+1);
        } else {
            error("expect )");
        }
    } else {
        error("expect id num or (");
    }
}

void print_blank(int blank) {
    // when blank == 0, same as 1;
    printf("%*c", blank, ' ');
}

// remove space in parse_string
void remove_blank() {
    char tmpstr[max_len] = "\0";
    for (int i=0, j=0; parse_string[i]; ++i) {
        if (parse_string[i] != ' ' && parse_string[i] != '\t'
                && parse_string[i] != '\r' && parse_string[i] != '\n') {
            tmpstr[j++] = parse_string[i];
        }
    }
    for (int i=0; i<max_len; ++i) {
        parse_string[i] = tmpstr[i];
    }
    printf("%s\n", parse_string);
}

void getChar() {
    parse_idx += 1;
    now_char = parse_string[parse_idx];
    if (now_char >= '0' && now_char <= '9' ) {
        char_class = DIGIT;
    } else if (now_char >= 'a' && now_char <= 'z') {
        char_class = LETTER;
    } else if (now_char >= 'A' && now_char <= 'Z') {
        char_class = LETTER;
    } else if (now_char == '_') {
        char_class = LETTER;
    } else if (now_char == '(') {
        char_class = LEFT_PAR;
    } else if (now_char == ')') {
        char_class = RIGHT_PAR;
    } else if (now_char == '+') {
        char_class = PLUS;
    } else if (now_char == '-') {
        char_class = MINUS;
    } else if (now_char == '*') {
        char_class = MULT;
    } else if (now_char == '/') {
        char_class = DIVI;
    } else if (now_char == ';') {
        char_class = SCOLON;
    } else if (now_char == '=') {
        char_class = EQ;
    } else if (now_char == '{') {
        char_class = LEFT_BRACE;
    } else if (now_char == '}') {
        char_class = RIGHT_BRACE;
    } else if (now_char == '>') {
        char_class = MORE;
    } else if (now_char == '<') {
        char_class = LESS;
    } else if (now_char == '&') {
        char_class = AND;
    } else if (now_char == '|') {
        char_class = OR;
    } else if (now_char == '\0') {
        char_class = END;
    } else {
        char_class = UNKOWN;
    }
}

void addChar() {
    buffer[buf_idx++] = now_char;
}

void lex() {
    getChar();
    switch (char_class) {
        case LETTER:
            while (char_class == LETTER || char_class == DIGIT) {
                addChar();
                getChar();
            }
            // get a more char
            parse_idx -= 1;
            break;
        case DIGIT:
            addChar();
            getChar();
            while (char_class == DIGIT) {
                addChar();
                getChar();
            }
            // get a more char
            parse_idx -= 1;
            break;
        case PLUS:
            addChar();
            break;
        case MINUS:
            addChar();
            break;
        case MULT:
            addChar();
            break;
        case DIVI:
            addChar();
            break;
        case LEFT_PAR:
            addChar();
            break;
        case RIGHT_PAR:
            addChar();
            break;
        case SCOLON:
            addChar();
            break;
        case EQ:
            addChar();
            break;
        case LEFT_BRACE:
            addChar();
            break;
        case RIGHT_BRACE:
            addChar();
            break;
        case MORE:
            addChar();
            break;
        case LESS:
            addChar();
            break;
        case AND:
            addChar();
            break;
        case OR:
            addChar();
            break;
        default:
            break;
    }
}

void look_next() {
    // peek next char in parse_string
    char next_char = parse_string[parse_idx+1];
    if (next_char == '(') {
        next_token = LEFT_PAREN_CODE;
    } else if (next_char == ')') {
        next_token = RIGHT_PAREN_CODE;
    } else if (next_char == '+') {
        next_token = PLUS_CODE;
    } else if (next_char == '-') {
        next_token = MINUS_CODE;
    } else if (next_char == '*') {
        next_token = MULT_CODE;
    } else if (next_char == '/') {
        next_token = DIVI_CODE;
    } else if (next_char == '\0') {
        next_token = END_CODE;
    } else if (next_char >= '0' && next_char <= '9' ) {
        next_token = NUM_CODE;
    } else if (next_char >= 'a' && next_char <= 'z') {
        next_token = ID_CODE;
    } else if (next_char >= 'A' && next_char <= 'Z') {
        next_token = ID_CODE;
    } else if (next_char == '_') {
        next_token = ID_CODE;
    } else if (next_char == ';') {
        next_token = SCOLON_CODE;
    } else if (next_char == '{') {
        next_token = LEFT_BRACE_CODE;
    } else if (next_char == '}') {
        next_token = RIGHT_BRACE_CODE;
    } else if (next_char == '=') {
        next_token = EQ_CODE;
    } else if (next_char == '>') {
        next_token = MORE_CODE;
    } else if (next_char == '<') {
        next_token = LESS_CODE;
    } else if (next_char == '&') {
        next_token = AND_CODE;
    } else if (next_char == '|') {
        next_token = OR_CODE;
    } else {
        next_token = UNKOWN_CODE;
    }
}

// print token from lex
void printlex(int blank) {
    print_blank(blank);
    printf("%s\n", buffer);
    for (int i=0; i<buf_idx; ++i)
        buffer[i] = '\0';
    buf_idx = 0;
}
