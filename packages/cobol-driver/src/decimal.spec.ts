import { CopybookInterpreter } from "./interpreter/copybook.interpreter";
import { CobolToEcmaService } from "./service/cobol-to-ecma.service";

describe('Testes relacionados a validação da interpretação de decimais:', () => {
    it('recebendo um valor inteiro no local onde se recebe um decimal', () => {
        const content = `
        03 DADOS.
           05 SQLCODE-RETORNO           PIC  9(003)V999 VALUE ZERO.
           05 MSG-RETORNO               PIC  9(003)V9(2) VALUE ZERO.
           05 FILLER                    PIC  X(005) VALUE SPACE.
           05 QTD-OCORR                 PIC  9(001) VALUE ZERO.
`;
        const expectation = {
            "DADOS": {
                "SQLCODE-RETORNO": 3,
                "MSG-RETORNO": 0,
                "FILLER": "",
                "QTD-OCORR": 0
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "00300000000     0");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('enviando valor com uma casa decimal', () => {
        const content = `
        03 DADOS.
           05 SQLCODE-RETORNO           PIC  9(003)V999 VALUE ZERO.
           05 MSG-RETORNO               PIC  9(003)V9(2) VALUE ZERO.
           05 FILLER                    PIC  X(005) VALUE SPACE.
           05 QTD-OCORR                 PIC  9(001) VALUE ZERO.
`;
        const expectation = {
            "DADOS": {
                "SQLCODE-RETORNO": 3.2,
                "MSG-RETORNO": 0,
                "FILLER": "",
                "QTD-OCORR": 0
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "00320000000     0");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('enviando valor com duas casa decimal', () => {
        const content = `
        03 DADOS.
           05 SQLCODE-RETORNO           PIC  9(003)V999 VALUE ZERO.
           05 MSG-RETORNO               PIC  9(003)V9(2) VALUE ZERO.
           05 FILLER                    PIC  X(005) VALUE SPACE.
           05 QTD-OCORR                 PIC  9(001) VALUE ZERO.
`;
        const expectation = {
            "DADOS": {
                "SQLCODE-RETORNO": 3.22,
                "MSG-RETORNO": 0,
                "FILLER": "",
                "QTD-OCORR": 0
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "00322000000     0");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('enviando valor com todas as casas decimais', () => {
        const content = `
        03 DADOS.
           05 SQLCODE-RETORNO           PIC  9(003)V999 VALUE ZERO.
           05 MSG-RETORNO               PIC  9(003)V9(2) VALUE ZERO.
           05 FILLER                    PIC  X(005) VALUE SPACE.
           05 QTD-OCORR                 PIC  9(001) VALUE ZERO.
`;
        const expectation = {
            "DADOS": {
                "SQLCODE-RETORNO": 3.222,
                "MSG-RETORNO": 0,
                "FILLER": "",
                "QTD-OCORR": 0
            }
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, "00322200000     0");

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });

    it('testando todas as possibilidades de interpretação de decimais', () => {
        const content = `
        03 DADOS.
           05 REAL-DECIMAL-1         PIC  99.99 VALUE ZERO.
           05 REAL-DECIMAL-2         PIC  9(3).9(2) VALUE ZERO.
           05 IMPLIED-DECIMAL-1      PIC  9(003)V99 VALUE ZERO.
           05 IMPLIED-DECIMAL-2      PIC  9(003)V9(2) VALUE ZERO.
`;

        const expectation = [{ "line": 1, "level": 3, "bookName": "unamed", "occurs": null, "metatype": { "type": "object", "binary": false, "length": false, "decimal": false, "signed": false }, "attributes": [{ "line": 2, "level": 5, "bookName": "unamed", "occurs": null, "metatype": { "type": "number", "binary": false, "length": 3, "decimal": 2, "signed": false }, "attributes": null, "name": "REAL-DECIMAL-1" }, { "line": 3, "level": 5, "bookName": "unamed", "occurs": null, "metatype": { "type": "number", "binary": false, "length": 3, "decimal": 2, "signed": false }, "attributes": null, "name": "REAL-DECIMAL-2" }, { "line": 4, "level": 5, "bookName": "unamed", "occurs": null, "metatype": { "type": "number", "binary": false, "length": 3, "decimal": 2, "signed": false }, "attributes": null, "name": "IMPLIED-DECIMAL-1" }, { "line": 5, "level": 5, "bookName": "unamed", "occurs": null, "metatype": { "type": "number", "binary": false, "length": 3, "decimal": 2, "signed": false }, "attributes": null, "name": "IMPLIED-DECIMAL-2" }], "name": "DADOS" }];
        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');

        expect(JSON.stringify(expectation)).toBe(JSON.stringify(structure));
    });
});