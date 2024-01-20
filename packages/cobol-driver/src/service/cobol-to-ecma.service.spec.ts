import { CopybookInterpreter } from "../interpreter/copybook.interpreter";
import { CobolToEcmaService } from "./cobol-to-ecma.service";

describe('Cobol-to-Ecma: testing convertion to JSON', () => {
  it('Creating empty JSON', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
          10  CODIGO            PIC 9(017) VALUE ZEROS.
          10  NOME              PIC X(010) VALUE SPACES.
          10  DESCRICAO         PIC X(002) VALUE SPACES.
          10  ARTEFATP          PIC X(010) VALUE SPACES.
          10  EMPRESA           PIC 9(004) VALUE ZEROS.
          10  FUNCIONARIO       PIC 9(009) VALUE ZEROS.`;

    const expectedResult = {
      "ACAO": 0,
      "DADOS": {
        "CODIGO   ": 0,
        "NOME    ": '',
        "DESCRICAO": '',
        "ARTEFATP": '',
        "EMPRESA": 0,
        "FUNCIONARIO": 0
      }
    };

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 2');
    const json = CobolToEcmaService.getInstance().convert(metaModels);
    expect(json).toEqual(expectedResult);
  });

  it('Creating empty string', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
         10  CODIGO            PIC S9(017) VALUE ZEROS.
         10  TERCEIRO          PIC X(010) VALUE SPACES.
         10  TIPO              PIC X(002) VALUE SPACES
`;

    const expectedResult = {
      "ACAO": 0,
      "DADOS": {
        "CODIGO": 0,
        "TERCEIRO": '',
        "TIPO": ''
      }
    };

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 2');
    const json = CobolToEcmaService.getInstance().convert(metaModels);
    expect(json).toEqual(expectedResult);
  });

  it('Creating meta model from string with data', () => {
    const book =
      `
      05  ACAO                PIC 9(002).
      05  DADOS.
         10  SALDO             PIC S9(017) VALUE ZEROS.
         10  FONTE             PIC X(010) VALUE SPACES.
         10  STATUS            PIC X(002) VALUE SPACES
`;

    const prefilledData = '010000000000000543FTESTE     A ';

    const metaModels = CopybookInterpreter.getInstance().interpret(book, 'test 5');
    const json = CobolToEcmaService.getInstance().convert(metaModels, prefilledData);
    expect(json).toEqual({
      "ACAO": 1,
      "DADOS": {
        "SALDO": 5436,
        "FONTE": 'TESTE',
        "STATUS": 'A',
      }
    });
  });

  it('Creating meta model with complex scenarios book', () => {
    const book = `
      *----------------------------------------------------------------*
      *  DESCRICAO DE MEU COPYBOOK                                     *
TESTE *  ETC ETC ETC                                                   *
      *----------------------------------------------------------------*
        03 SAIDA.
             05 RETORNO.
      *    RETORNO DO PROCESSAMENTO                                    *
              10 CODIGO          PIC  9(004).
      *          CODIGO DE RETORNO                                     *
              10 QUANTIDADE     PIC  9(003).
      *          SQLCODE DE RETORNO (-999)                             *
              10 MENSAGEM         PIC  X(100).
      *          MENSAGEM DE RETORNO                                   *
              10 FILLER              PIC  X(200).
      *          FILLER PARA EXPANSAO                                  *
           05 SAIDA.
              10 CODIGO           PIC  S9(017).
      *          NUMERO DA PRE-VENDA                                   *
              10 DATA           PIC  X(010).
      *          DATA DO REGISTRO DA PRE-VENDA                         *
              10 SITUACAO           PIC  X(001).
      *          SITUACAO DA PRE-VENDA                                 *
      *          * L - AGUARDANDO LIBERACAO                            *
      *          * A - ABERTA                                          *
      *          * P - EM PROCESSAMENTO                                *
      *          * F - FINALIZADO                                      *
      *          * C - CANCELADA                                       *
              10 FORNECEDOR              PIC  9(009).
      *          CODIGO DO CLIENTE DA PRE-VENDA                        *
              10 CUPOM       PIC  9(011).
      *          VALOR TOTAL DA PRE-VENDA                              *
              10 CUPOM-DIGITO   PIC  9(011).
      *          VALOR DOS DESCONTOS DIGITADOS                         *
              10 PACOTE   PIC  9(011).
      *          VALOR DOS DESCONTOS OBRIGATORIOS                      *
              10 DESCONTO   PIC  9(011).
      *          VALOR TOTAL DE DESCONTOS                              *
              10 MULTA   PIC  9(011).
      *          VALOR TOTAL DE ACRESCIMOS                             *
              10 JUROS       PIC  9(009).
      *          VALOR DOS JUROS POR CONTA DO EMISSOR                  *
              10 TOTAL   PIC  9(011).
      *          VALOR LIQUIDO DA PRE-VENDA                            *
              10 TETANT              PIC  9(004).
      *          CODIGO DA BANDEIRA                                    *
              10 PARCEIRO   PIC  9(004).
      *          CODIGO DA EMPRESA                                     *
              10 LOJA      PIC  9(004).
      *          CODIGO DA FILIAL                                      *
              10 FILLER              PIC  X(050).
      *          FILLER PARA EXPANSAO                                  *`;

    let metaModels: any;
    try {
      metaModels = CopybookInterpreter.getInstance().interpret(book, 'noname');
    } catch (e) {
      console.error(e);
      throw e;
    }

    const json = CobolToEcmaService.getInstance().convert(metaModels);
    expect(json).toEqual({
      "SAIDA": {
        "RETORNO": {
          "CODIGO": 0,
          "QUANTIDADE": 0,
          "MENSAGEM": "",
          "FILLER": ""
        },
        "SAIDA": {
          "CODIGO": 0,
          "DATA": "",
          "SITUACAO": "",
          "FORNECEDOR": 0,
          "CUPOM": 0,
          "CUPOM-DIGITO": 0,
          "PACOTE": 0,
          "DESCONTO": 0,
          "MULTA": 0,
          "JUROS": 0,
          "TOTAL": 0,
          "TETANT": 0,
          "PARCEIRO": 0,
          "LOJA": 0,
          "FILLER": ""
        }
      }
    });
  });
});
