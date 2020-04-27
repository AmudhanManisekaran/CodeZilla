from sly import Lexer
import simplejson

class CalcLexer(Lexer):
    tokens = { NUMBER, ID, WHILE, IF, ELSE, PRINT, START, SEMICOLON, VAR,
               PLUS, MINUS, TIMES, DIVIDE, ASSIGN, STRING, ENDFOR, ENDTERNARY, ENDSHOW, ENDREAD,
               EQ, LT, LE, GT, GE, NE, FOR, ENDWHILE, ENDIF, SHOW, DO, END, LTA, GTA ,GTE,LTE,SS}

    literals = { '[',']','(', ')', '{', '}', ';', ',', ':', '\'', ':=' ,'.','$','#','@'}

    # String containing ignored characters
    ignore = ' \t'

    # Regular expression rules for tokens
    PLUS    = r'\+'
    MINUS   = r'-'
    TIMES   = r'\*'
    DIVIDE  = r'/'
    EQ      = r'=='
    ASSIGN  = r'='
    LE      = r'<='
    LT      = r'<'
    GE      = r'>='
    GT      = r'>'
    NE      = r'!='
    SS      = r'<<'
    SEMICOLON = ';'
    START = 'start'
    ENDIF = 'endif'
    ENDFOR = 'endfor'
    ENDTERNARY = 'endternary'
    ENDREAD = 'endread'
    ENDSHOW = 'endshow'
    ENDWHILE = 'endwhile'
    END = 'end'
    VAR = 'var'

    @_(r'\d+')
    def NUMBER(self, t):
        t.value = int(t.value)
        return t

    # Identifiers and keywords
    STRING = r'[a-zA-Z_][a-zA-Z_][a-zA-Z0-9_]*'
    ID = r'[a-zA-Z_]'
    ID['if'] = IF
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['print'] = PRINT
    ID['for'] = FOR
    # ID['endwhile'] = ENDWHILE
    # ID['endif'] = ENDIF
    ID['show'] = SHOW
    ID['do'] = DO
    ID['<<'] = LTA
    ID['>>'] = GTA
    ID['>='] = GTE
    ID['<='] = LTE

    ignore_comment = r'\#.*'

    # Line number tracking
    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += t.value.count('\n')

    def error(self, t):
        self.index += 1

if __name__ == '__main__':

    print("\n****************Start Execution****************\n")
    inputFile = 'pro.cz'
    string_concat = ""
    str = open(inputFile, 'r').read()
    arr = []
    lexer = CalcLexer()
    string = 0

    for tok in lexer.tokenize(str):
        print(tok)

    for tok in lexer.tokenize(str):

        if tok.type != 'STRING':
            if string == 0:
                arr.append(tok.value)
            elif string == 1:
                string_concat=string_concat[:-1]
                arr.append(string_concat)
                string_concat=""
                arr.append(tok.value)
                string = 0

        if tok.type == 'STRING':
            string = 1
            string_concat+=tok.value
            string_concat+='_'

    f = open('output.tok','w')
    simplejson.dump(arr,f)
    f.close()

    f = open('output.tok','r')
    arr =  []
    result = []
    arr = f.read()
    str1=""
    str2=""

    for x in arr:
        x = x.replace('"','')
        str1+=x

    for x in str1:
        x = x.replace(';','semicolon')
        x = x.replace('(','open_para')
        x = x.replace(')','close_para')
        x = x.replace(':','colon')
        x = x.replace('=','equal')
        x = x.replace('<<','<<"')
        x = x.replace('>>','">>')
        x = x.replace('<','less_than')
        x = x.replace('>','greater_than')
        x = x.replace(']','].')
        str2+=x

    file = open('result.tok','w')
    file.write(str2)
    file.close()

    print(str2)
    print("\n****************End Of Execution****************")
