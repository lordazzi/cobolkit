import { InvalidJsonException } from "./invalid-json.exception";
import { MetaValue } from "../model/meta-value.type";

export class InvalidJsonFactory {

    private constructor() {}

    private static getEcmaTypeName(ecmaValue: MetaValue | undefined): string {
        let typeName: string = typeof ecmaValue;
        if (typeName === 'object') {
            if (ecmaValue == null) {
                typeName = 'null';
            } else {
                typeName = (ecmaValue as Object).constructor.name;
            }
        } else if (typeName === 'function' && (ecmaValue as any as Function).name) {
            typeName = `function named ${(ecmaValue as any as Function).name}`
        }

        return typeName;
    }

    static ecmaArrayExpected(
        fieldName: string, ecmaValue: MetaValue | undefined
    ): InvalidJsonException {
        if (ecmaValue == null || !(ecmaValue instanceof Array)) {
            return new InvalidJsonException(fieldName,
                `Array was expected, but ${InvalidJsonFactory.getEcmaTypeName(ecmaValue)} was given`
            );
        } else {
            return new InvalidJsonException(fieldName);
        }
    }
    
    static malformedEcmaArray(
        fieldName: string, lengthExpected: number, ecmaValue: Array<any>,
        dependingOnFieldName?: string
    ): InvalidJsonException {
        if (dependingOnFieldName) {
            return new InvalidJsonException(fieldName,
                `ecma array with length ${ecmaValue.length} given,
                but ${lengthExpected} was expected`
            );
        } else {
            return new InvalidJsonException(fieldName,
                `ecma array with length different from length orchestrator
                "${dependingOnFieldName}". ${lengthExpected} was expected and
                ${ecmaValue.length} was given`
            );
        }
    }

    static ecmaStringExpected(
        fieldName: string, ecmaValue: MetaValue | undefined
    ): InvalidJsonException {
        return new InvalidJsonException(fieldName,
            `string was expected, but ${InvalidJsonFactory.getEcmaTypeName(ecmaValue)} was given`
        );
    }

    static invalidAlphabeticValue(
        fieldName: string, ecmaValue: string
    ): InvalidJsonException {
        return new InvalidJsonException(fieldName,
            `invalid alphabetic value. Only a to z characteres and spaces are accept,
            "${ecmaValue}" was given`
        );
    }
    
    static invalidStringLength(
        fieldName: string, ecmaValue: string, maxLength: number
    ): InvalidJsonException {
        return new InvalidJsonException(fieldName,
            `${fieldName} has surpassed the maxlength of ${
                maxLength
            } with value "${ecmaValue}" (length: ${ecmaValue.length})`
        );
    }
}