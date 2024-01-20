import { CobolToEcmaService } from './service/cobol-to-ecma.service';
import { CopybookInterpreter } from "./interpreter/copybook.interpreter";

describe('', () => {
    it('Testando geração de dados com occurs', () => {
        const content = `
            05 LINHA-ITENS OCCURS 3 TIMES.
               10 QUANTIDADE       PIC 9(2).
               10 DESCRICAO        PIC X(5).
               10 PRECO-UNITARIO   PIC S9(3)V9(2).
`;
        const expectation = {
            "LINHA-ITENS": [
                { "QUANTIDADE": 0, "DESCRICAO": "", "PRECO-UNITARIO": 0 },
                { "QUANTIDADE": 0, "DESCRICAO": "", "PRECO-UNITARIO": 0 },
                { "QUANTIDADE": 0, "DESCRICAO": "", "PRECO-UNITARIO": 0 }
            ]
        };

        const structure = CopybookInterpreter.getInstance().interpret(content, 'book');
        const metaValue = CobolToEcmaService.getInstance().convert(structure, '');

        expect(JSON.stringify(metaValue)).toBe(JSON.stringify(expectation));
    });
});