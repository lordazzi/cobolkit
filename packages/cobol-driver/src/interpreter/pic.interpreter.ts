import { CobolDataType } from "../model/cobol-data-type.enum";
import { InvalidBookFactory } from "../business/invalid-book.factory";
import { MetaModelStructure } from "../model/meta-model-structure.model";

export class PicInterpreter {

    static readonly NUMBER_WRAPPED_IN_PARENTHESES = /\(\d+\)/;
    static readonly FIRST_AND_LAST_CHAR = /^.|.$/g;
    static readonly COMMAND_DATA_TYPE = /X|A|9/;
    static readonly GET_IF_SIGNED = /S/;
    static readonly GET_IF_DECIMAL = /V/;

    static readonly IS_PARENTHESES_DECIMAL_ORIENTED = /V9\(/;
    static readonly BEGIN_TO_A_V9 = /^.*V9\(/;
    static readonly NOT_A_NUMBER_TO_THE_END = /[^0-9].*$/;

    static readonly IS_NINE_DECIMAL_ORIENTED = /V9+/;
    static readonly BEGIN_TO_A_V = /^.*V/;
    static readonly NOT_A_NINE_TO_THE_END = /[^9].*$/;

    private static instance: PicInterpreter;

    static getInstance(): PicInterpreter {
        if (!this.instance) {
            this.instance = new PicInterpreter();
        }
        return this.instance;
    }

    private constructor() { }

    interpret(metaDataArgument: MetaModelStructure, picCommand: string): void {
        const typeChar = this.getTypeChar(metaDataArgument, picCommand);
        if (typeChar === '9') {
            metaDataArgument.metatype.type = CobolDataType.NUMBER;
            this.readPicAsNumber(metaDataArgument, picCommand);
        } else if (typeChar === 'X') {
            metaDataArgument.metatype.type = CobolDataType.CHARACTERES;
        } else if (typeChar === 'A') {
            metaDataArgument.metatype.type = CobolDataType.ALPHABETIC;
        }

        metaDataArgument.metatype.length = this.getLength(picCommand);
    }

    private getTypeChar(metaData: MetaModelStructure, picCommand: string): 'A' | 'X' | '9' {
        const typeMatches = picCommand.match(PicInterpreter.COMMAND_DATA_TYPE);
        const typeMatch = typeMatches && typeMatches[0];

        if (!typeMatch) {
            throw InvalidBookFactory.invalidPic(metaData.line, picCommand);
        }

        return typeMatch as 'A' | 'X' | '9';
    }

    private readPicAsNumber(metaData: MetaModelStructure, picCommand: string): void {
        metaData.metatype.signed = !!picCommand.match(PicInterpreter.GET_IF_SIGNED);

        if (!!picCommand.match(PicInterpreter.GET_IF_DECIMAL)) {
            let match: string;

            //  extract info from V9(n) pattern
            if (picCommand.match(PicInterpreter.IS_PARENTHESES_DECIMAL_ORIENTED)) {
                match = picCommand.replace(PicInterpreter.BEGIN_TO_A_V9, '');
                match = match.replace(PicInterpreter.NOT_A_NUMBER_TO_THE_END, '');

                //  extract info from V999 pattern
            } else if (picCommand.match(PicInterpreter.IS_NINE_DECIMAL_ORIENTED)) {
                match = picCommand.replace(PicInterpreter.BEGIN_TO_A_V, '');
                match = match.replace(PicInterpreter.NOT_A_NINE_TO_THE_END, '');
                match = String(match.length);

            } else { // not a known pattern
                throw InvalidBookFactory.invalidPic(metaData.line, picCommand);
            }

            metaData.metatype.decimal = Number(match);
        }
    }

    private getLength(picString: string): number {
        const matches = picString.match(PicInterpreter.NUMBER_WRAPPED_IN_PARENTHESES);
        const match = matches && matches[0];

        if (!match) {
            //  1 is the default value when the field has no size specification
            return 1;
        }

        return Number(match.replace(PicInterpreter.FIRST_AND_LAST_CHAR, ''));
    }
}