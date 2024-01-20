import { IterableString } from '@belomonte/iterable-string';
import { InternalDriverException } from "../business/internal-driver.exception";
import { InvalidPositionalStringException } from '../business/invalid-positional-string.exception';
import { CobolDataType } from "../model/cobol-data-type.enum";
import { MetaModelStructure } from "../model/meta-model-structure.model";
import { MetaModel } from './../model/meta-model.type';
import { MetaValue } from './../model/meta-value.type';

export class CobolToEcmaService {
  private static instance: CobolToEcmaService;

  static LAST_CHAR = /.$/;

  static getInstance() {
    if (!this.instance) {
      this.instance = new CobolToEcmaService();
    }

    return this.instance;
  }

  private readonly SINGN: {
    [prop: string]: {
      value: string,
      sign: 1 | -1
    }
  } = {
      '{': { value: '0', sign: 1 },
      'A': { value: '1', sign: 1 },
      'B': { value: '2', sign: 1 },
      'C': { value: '3', sign: 1 },
      'D': { value: '4', sign: 1 },
      'E': { value: '5', sign: 1 },
      'F': { value: '6', sign: 1 },
      'G': { value: '7', sign: 1 },
      'H': { value: '8', sign: 1 },
      'I': { value: '9', sign: 1 },
      '}': { value: '0', sign: -1 },
      'J': { value: '1', sign: -1 },
      'K': { value: '2', sign: -1 },
      'L': { value: '3', sign: -1 },
      'M': { value: '4', sign: -1 },
      'N': { value: '5', sign: -1 },
      'O': { value: '6', sign: -1 },
      'P': { value: '7', sign: -1 },
      'Q': { value: '8', sign: -1 },
      'R': { value: '9', sign: -1 }
    };

  private constructor() { }

  convert(meta: MetaModelStructure[], positionalString?: IterableString | string): MetaValue {
    let iterablePositional: IterableString | undefined;
    let context: { [prop: string]: number | string } = {};
    let metaValue: { [attr: string]: MetaValue } = {};

    if (positionalString instanceof IterableString) {
      iterablePositional = positionalString;
    } else if (typeof positionalString === 'string') {
      iterablePositional = new IterableString(positionalString);
    }

    meta.forEach((metaModelStructure) => {
      const attrName = metaModelStructure.name || 'FILLER';
      metaValue[attrName] = this.positionalStructureToEcma(
        metaModelStructure, iterablePositional || new IterableString(''), context
      );
    });

    return metaValue;
  }

  private positionalStructureToEcma(
    meta: MetaModelStructure, positionalString: IterableString,
    context: { [prop: string]: number | string }
  ): MetaValue {
    if (meta.occurs) {
      return this.occursStructureToEcmaArray(meta, positionalString, context || {});
    } else {
      return this.nonOccursStructureToEcma(meta, positionalString, context || {});
    }
  }

  private nonOccursStructureToEcma(
    meta: MetaModelStructure, positionalString: IterableString,
    context: { [prop: string]: number | string }
  ): string | number | MetaModel {
    switch (meta.metatype.type) {
      case CobolDataType.ALPHABETIC:
        return this.alphabeticToEcmaString(meta, positionalString, context || {});
      case CobolDataType.CHARACTERES:
        return this.characteresToEcmaString(meta, positionalString, context || {});
      case CobolDataType.NUMBER:
        return this.cobolNumberToEcmaNumber(meta, positionalString, context || {});
      case CobolDataType.OBJECT:
        return this.cobolStructureToEmcaObject(meta, positionalString, context || {});
      default:
        throw new InternalDriverException();
    }
  }

  private occursStructureToEcmaArray(
    meta: MetaModelStructure, positionalString: IterableString,
    context: { [prop: string]: number | string }
  ): MetaValue {
    if (!meta.occurs) {
      throw new InternalDriverException();
    }

    let length = meta.occurs.minLength || meta.occurs.maxLength || 0;
    const array: (string | number | MetaModel)[] = [];
    if (meta.occurs.dependingOn) {
      const dependingOn = context[meta.occurs.dependingOn];
      if (typeof dependingOn !== 'number') {
        throw new InvalidPositionalStringException(0, 0, `Invalid depending on attribute`);
      }

      length = dependingOn;
      if (meta.occurs.minLength && length < meta.occurs.minLength) {
        throw new InvalidPositionalStringException(0, 0, `Invalid number given as depending on (too low)`);
      } else if (meta.occurs.maxLength && length > meta.occurs.maxLength) {
        throw new InvalidPositionalStringException(0, 0, `Invalid number given as depending on (too big)`);
      }
    }

    for (let i = 0; i < length; i++) {
      array.push(
        this.nonOccursStructureToEcma(meta, positionalString, context)
      );
    }

    return array;
  }

  private alphabeticToEcmaString(
    meta: MetaModelStructure, positionalString: IterableString | undefined,
    context: { [prop: string]: number | string }
  ): string {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    if (positionalString) {
      return context[meta.name] =
        positionalString.addCursor(meta.metatype.length).trim();
    } else {
      return context[meta.name] = meta.metatype.default as string || '';
    }
  }

  private characteresToEcmaString(
    meta: MetaModelStructure, positionalString: IterableString | undefined,
    context: { [prop: string]: number | string }
  ): string {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    if (positionalString) {
      return context[meta.name] =
        positionalString.addCursor(meta.metatype.length).trim();
    } else {
      return context[meta.name] = meta.metatype.default as string || '';
    }
  }

  private cobolNumberToEcmaNumber(
    meta: MetaModelStructure, positionalString: IterableString | undefined,
    context: { [prop: string]: number | string }
  ): number {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    if (positionalString) {
      const currentCursorPosition = positionalString.currenPosition;
      let numericString = positionalString.addCursor(meta.metatype.length);
      let ecmaNumeric = 0;

      if (meta.metatype.signed) {
        let lastChar: string | undefined = numericString[numericString.length - 1];
        //  case there is no string positional and
        //  this is a empty data generation execution
        if (!numericString && !lastChar) {
          lastChar = '{'; // than, it turns into a 'positive zero'
        }

        //  If there is a different character representing a sign
        if (!lastChar || !this.SINGN[lastChar]) {
          throw new InvalidPositionalStringException(0, 0,
            `Invalid signed number in positional string: ${numericString}`
          );
        }

        numericString = numericString.replace(
          CobolToEcmaService.LAST_CHAR, this.SINGN[lastChar].value
        );
        ecmaNumeric = Number(numericString.trim());
        ecmaNumeric = ecmaNumeric * this.SINGN[lastChar].sign;
      } else {
        ecmaNumeric = Number(numericString.trim());
      }

      if (meta.metatype.decimal) {
        let decimal = positionalString.addCursor(meta.metatype.decimal);
        ecmaNumeric = Number(`${ecmaNumeric}.${decimal}`);
      }

      if (isNaN(ecmaNumeric)) {
        throw new InvalidPositionalStringException(
          currentCursorPosition, positionalString.currenPosition, `Non numeric char found in position reserved to numerical data at the positional string`
        );
      }

      return context[meta.name] = ecmaNumeric;
    } else {
      return context[meta.name] = meta.metatype.default as number || 0;
    }
  }

  private cobolStructureToEmcaObject(
    meta: MetaModelStructure, positionalString: IterableString,
    context: { [prop: string]: number | string }
  ): MetaModel {
    if (!meta.attributes) {
      throw new InternalDriverException();
    }

    const object: MetaModel = {};
    meta.attributes.forEach(attr => {
      if (attr.name) {
        object[attr.name] = this.positionalStructureToEcma(attr, positionalString, context);
      }
    });

    return object;
  }
}