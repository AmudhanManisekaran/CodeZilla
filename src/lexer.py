from sly import Lexer
import simplejson

class CalcLexer(Lexer):
    tokens = { NUMBER, ID, WHILE, IF, ELSE, PRINT,
               PLUS, MINUS, TIMES, DIVIDE, ASSIGN,
               EQ, LT, LE, GT, GE, NE, FOR, ENDWHILE, ENDIF, SHOW, DO, END, LTA, GTA ,GTE,LTE}

    literals = { '[',']','(', ')', '{', '}', ';', ',', ':', '\'', ':=' ,'.','$'}

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


    @_(r'\d+')
    def NUMBER(self, t):
        t.value = int(t.value)
        return t

    # Identifiers and keywords
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['if'] = IF
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['print'] = PRINT
    ID['for'] = FOR
    ID['endwhile'] = ENDWHILE
    ID['endif'] = ENDIF
    ID['show'] = SHOW
    ID['do'] = DO
    ID['End'] = END
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
        # print('Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1

if __name__ == '__main__':

    print("\n****************Start Execution****************\n")
    # inputFile = input('Lexer > ')
    inputFile = 'pro.cz'

    str = open(inputFile, 'r').read()
    arr = []
    lexer = CalcLexer()
    for tok in lexer.tokenize(str):
       arr.append(tok.value)
    f = open('output.tok','w')
    simplejson.dump(arr,f)
    f.close()

    f = open('output.tok','r')
    arr =  []
    result = []
    arr = f.read()
    str1=""
    str2=""
    str3=""

    print(arr)
    
    for x in arr:
        x = x.replace('"','')
        str1+=x

    print(str1)

    for x in str1:
        x = x.replace('<','<"')
        str2+=x

    print(str2)

    for x in str2:
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
        str3+=x

    file = open('result.tok','w')
    file.write(str3)
    file.close()

    print(str3)
    print("\n****************End Of Execution****************")
