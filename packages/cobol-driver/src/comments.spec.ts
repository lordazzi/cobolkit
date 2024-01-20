import { CopybookInterpreter } from "./interpreter/copybook.interpreter";
import { CobolToEcmaService } from "./service/cobol-to-ecma.service";

describe('Testes relacionados a interpretação de books com comentários escritos de formas inconvenientes:', () => {
    it('comentário de bloco pulando linha', () => {
        const content = `
       * COMENTARIO UM
       * COMENTARIO DOIS

       * COMENTARIO TRÊS
        03 PESSOA.
           05 IDADE                PIC 9(03) VALUE ZEROS.
           05 NOME                 PIC X(10) VALUE SPACES.
           05 DOCUMENTO            PIC X(11) VALUE SPACES.
           05 TELEFONE.
               07 TELEFONE-DDD     PIC X(02) VALUE ZEROS.
               07 TELEFONE-NUMERO  PIC X(09) VALUE ZEROS.
`;
        const expectation = {
            "PESSOA": {
                "IDADE": 30,
                "NOME": "AZE",
                "DOCUMENTO": "41345678911",
                "TELEFONE": {
                    "TELEFONE-DDD": "11",
                    "TELEFONE-NUMERO": "932123113"
                }
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'comentado-1');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "030AZE       4134567891111932123113");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('comentário de bloco pulando linha, sendo que nesta linha há uma quantidade de caracteres inferior a 6', () => {
        const content = `
       * COMENTARIO UM
       * COMENTARIO DOIS
 ..
       * COMENTARIO TRÊS
        03 PESSOA.
           05 IDADE                PIC 9(03) VALUE ZEROS.
           05 NOME                 PIC X(10) VALUE SPACES.
           05 DOCUMENTO            PIC X(11) VALUE SPACES.
           05 TELEFONE.
               07 TELEFONE-DDD     PIC X(02) VALUE ZEROS.
               07 TELEFONE-NUMERO  PIC X(09) VALUE ZEROS.
`;
        const expectation = {
            "PESSOA": {
                "IDADE": 30,
                "NOME": "AZE",
                "DOCUMENTO": "41345678911",
                "TELEFONE": {
                    "TELEFONE-DDD": "11",
                    "TELEFONE-NUMERO": "932123113"
                }
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'comentado-2');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "030AZE       4134567891111932123113");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('informações alocadas na área reservada para o mainframe de forma irregular', () => {
        const content = `
AZZI    03 PESSOA.
 --        05 IDADE                PIC 9(03) VALUE ZEROS.
 --        05 NOME                 PIC X(10) VALUE SPACES.
 --        05 DOCUMENTO            PIC X(11) VALUE SPACES.
 --
 --        05 TELEFONE.
               07 TELEFONE-DDD     PIC X(02) VALUE ZEROS.
               07 TELEFONE-NUMERO  PIC X(09) VALUE ZEROS.
`;
        const expectation = {
            "PESSOA": {
                "IDADE": 30,
                "NOME": "AZE",
                "DOCUMENTO": "41345678911",
                "TELEFONE": {
                    "TELEFONE-DDD": "11",
                    "TELEFONE-NUMERO": "932123113"
                }
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'comentado-3');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "030AZE       4134567891111932123113");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });
});