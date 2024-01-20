import { InvalidBookException } from "./invalid-book.exception";
import { MetaModelStructure } from "../model/meta-model-structure.model";

export class InvalidBookFactory {

    private constructor() {}

    static invalidPic(line: number, pic: string): InvalidBookException {
        return new InvalidBookException(line, `Invalid PIC syntax`);
    }

    static invalidCommandLine(line: number, command: string): InvalidBookException {
        return new InvalidBookException(line, `Invalid command syntax`);
    }

    static unsupportedCommand(line: number, invalidKeyword: string, fullCommand: string): InvalidBookException {
        return new InvalidBookException(line, `Unsuportted command found: ${invalidKeyword}`);
    }

    static manDontDoThat(line: number, someCode: string): InvalidBookException {
        return new InvalidBookException(line, `Won't believe that you really doing this. The reader don't support this, sorry.`);
    }

    static noPropertiesObject(line: number): InvalidBookException {
        return new InvalidBookException(line, `You have created an object without properties.`);
    }

    static invalidBook(): InvalidBookException {
        return new InvalidBookException(0, `Failed to read book and failed to find where is the problem.`);
    }

    static multipleRootFound(bookName?: string): InvalidBookException {
        return new InvalidBookException(0, `The book "${bookName}" has more than one field and no root to make a group was found.`);
    }

    static occursWithLengthRangeButNoDependingOn(metaModel: MetaModelStructure): InvalidBookException {
        return new InvalidBookException(metaModel.line, `You're using TO operator in the book "${metaModel.bookName}" to create an length range in the occurs but a DEPENDING ON clause was not given.`);
    }

    static dependingOfAUndefinedValue(line: number): InvalidBookException {
        return new InvalidBookException(line, `DEPEDING ON attrbite was not found. If the attribute that define the length is after the occurs, the reader will not work. If it's binding to a non numeric value will not work either.`);
    }

    static dependingOfANonNumbericValue(line: number): InvalidBookException {
        return new InvalidBookException(line, `DEPEDING ON binding to a non numeric value.`);
    }
}