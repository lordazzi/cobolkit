import { CopybookInterpreter } from "../interpreter/copybook.interpreter";
import { EcmaToCobolService } from "./ecma-to-cobol.service";

describe('Ecma-to-Cobol: testing convertion to positional string', () => {
  it('Creating empty string', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
         10  CODIGO         PIC 9(017) VALUE ZEROS.
         10  TITLE          PIC X(010) VALUE SPACES.
         10  STATUS         PIC X(002) VALUE SPACES.
         10  DATA           PIC X(010) VALUE SPACES.
         10  STORE          PIC 9(004) VALUE ZEROS.
         10  SELLER         PIC 9(009) VALUE ZEROS.`;

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 3');
    const positionalData = EcmaToCobolService.getInstance().convert(metaModels);
    expect(positionalData).toBe('0000000000000000000                      0000000000000');

  });

  it('Creating empty string', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
         10  CODIGO        PIC S9(017) VALUE ZEROS.
         10  TITLE         PIC X(010) VALUE SPACES.
         10  STATUS        PIC X(002) VALUE SPACES
`;

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 4');
    const positionalData = EcmaToCobolService.getInstance().convert(metaModels);
    expect(positionalData).toBe('000000000000000000{            ');

  });

  it('Creating string from existing data', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
         10  CODIGO        PIC S9(017) VALUE ZEROS.
         10  TITLE         PIC X(010) VALUE SPACES.
         10  STATUS        PIC X(002) VALUE SPACES
`;

    const prefilledData = {
      "ACAO": 1,
      "DADOS": {
        "CODIGO": 5436,
        "TITLE": 'TESTE',
        "STATUS": 'A',
      }
    };

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 5');
    const json = EcmaToCobolService.getInstance().convert(metaModels, prefilledData);
    expect(json).toEqual('010000000000000543FTESTE     A ');
  });

  it('Creating meta model with complex scenarios book', () => {
    const book = `
      *----------------------------------------------------------------*
      *  EXEMPLO DE PROGRAMA DE SAIDA                                  *
TESTE *  EXEMPLO DE PROGRAMA DE SAIDA                                  *
      *----------------------------------------------------------------*
        03 SAIDA.
             05 RESPOSTA.
      *    EXEMPLO DE COMERNARIO                                    *
              10 RESPOSTA            PIC  9(004).
      *          RESPOSTA                                     *
              10 RETORNO             PIC  9(003).
      *          RETORNO                             *
              10 MENSAGEM            PIC  X(100).
      *          MENSAGEM                                   *
              10 FILLER              PIC  X(200).

           05 SAIDA.
              10 NUMERO-CHEQUE           PIC  S9(017).
      *          NUMERO DO CHEQUE                                   *
              10 DATA       PIC  X(010).
      *          DATA DO REGISTRO                         *
              10 SITUACAO             PIC  X(001).
              10 CLIENTE              PIC  9(009).
      *          CODIGO DO CLIENTE                         *
              10 TOTAL       PIC  9(011).
              10 DOCUMENTO   PIC  9(011).
              10 COMPRADOR   PIC  9(011).
              10 DESCRICAO   PIC  9(011).
              10 PROPORCAO   PIC  9(011).
              10 JUROS       PIC  9(009).
              10 LIQUIDO     PIC  9(011).
              10 TIPO        PIC  9(004).
              10 REGIAO      PIC  9(004).
              10 LOJA        PIC  9(004).
              10 FILLER      PIC  X(050).`

    let metaModels: any;
    try {
      metaModels = CopybookInterpreter.getInstance().interpret(book, 'cerveja');
    } catch (e) {
      console.error(e);
      throw e;
    }

    const positionalString = EcmaToCobolService.getInstance().convert(metaModels);
    expect(positionalString).toEqual("0000000                                                                                                                                                                                                                                                                                                            0000000000000000{           000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000                                                  ");
  });
});
