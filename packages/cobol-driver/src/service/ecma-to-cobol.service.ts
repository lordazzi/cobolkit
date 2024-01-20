/* eslint-disable @typescript-eslint/naming-convention */
import { InternalDriverException } from '../business/internal-driver.exception';
import { InvalidBookFactory } from '../business/invalid-book.factory';
import { InvalidJsonException } from '../business/invalid-json.exception';
import { InvalidJsonFactory } from '../business/invalid-json.factory';
import { CobolDataType } from '../model/cobol-data-type.enum';
import { MetaModelStructure } from '../model/meta-model-structure.model';
import { MetaValue } from '../model/meta-value.type';
import { EcmaValueValidator } from '../validator/ecma-value.validator';
import { MetaModel } from './../model/meta-model.type';

export class EcmaToCobolService {
  private static instance: EcmaToCobolService;

  static SIGN_N_THE_INTEGER_ONES = /^-?\d*(\.|$)/;
  static SIGN_N_THE_DECIMAL_ONES = /^-?|\.\d*$/g;
  static LAST_CHAR = /.$/;

  private readonly validator = EcmaValueValidator.getInstance();

  private readonly SINGNED_NEGATIVE: { [prop: number]: string } = {
    0: '{',
    1: 'A',
    2: 'B',
    3: 'C',
    4: 'D',
    5: 'E',
    6: 'F',
    7: 'G',
    8: 'H',
    9: 'I'
  };

  private readonly SINGNED_POSITIVE: { [prop: number]: string } = {
    0: '}',
    1: 'J',
    2: 'K',
    3: 'L',
    4: 'M',
    5: 'N',
    6: 'O',
    7: 'P',
    8: 'Q',
    9: 'R',
  };

  static getInstance(): EcmaToCobolService {
    if (!this.instance) {
      this.instance = new EcmaToCobolService();
    }

    return this.instance;
  }

  // eslint-disable-next-line @typescript-eslint/no-empty-function
  private constructor() { }

  convert(metaSctructures: MetaModelStructure[], metaModel?: MetaModel): string {
    let positionalString = '';
    metaSctructures.forEach(meta => {
      const ecmaValue = metaModel && metaModel[meta.name as string];
      positionalString += this.convertSingleRoot(meta, ecmaValue, {});
    });

    return positionalString;
  }

  private convertSingleRoot(
    meta: MetaModelStructure, ecmaValue?: MetaValue,
    context: { [prop: string]: number | string | undefined } = {}
  ): string {
    if (meta.occurs) {
      return this.ecmaArrayToOccursStructure(meta, ecmaValue, context);
    } else {
      return this.ecmaToPositionalString(meta, ecmaValue, context);
    }
  }

  private ecmaToPositionalString(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ): string {
    switch (meta.metatype.type) {
      case CobolDataType.ALPHABETIC:
        return this.ecmaStringToAlphabetic(meta, ecmaValue, context);
      case CobolDataType.CHARACTERES:
        return this.ecmaStringToCharacteres(meta, ecmaValue, context);
      case CobolDataType.NUMBER:
        return this.ecmaNumberToCobolNumber(meta, ecmaValue, context);
      case CobolDataType.OBJECT:
        return this.ecmaObjectToCobolStructure(meta, ecmaValue, context);
      default:
        throw new InternalDriverException();
    }
  }

  // eslint-disable-next-line complexity
  private ecmaArrayToOccursStructure(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ): string {
    if (!meta.occurs || !meta.name) {
      throw new InternalDriverException();
    }

    let length: number | undefined;
    if (meta.occurs.dependingOn) {
      const dependingOnValue = context[meta.occurs.dependingOn];
      if (dependingOnValue === undefined) {
        throw InvalidBookFactory.dependingOfAUndefinedValue(meta.line);
      } else if (typeof dependingOnValue !== 'number') {
        throw InvalidBookFactory.dependingOfANonNumbericValue(meta.line);
      }

      length = dependingOnValue;
    }

    if (length === undefined) {
      length = meta.occurs.minLength;
    }

    if (length === undefined) {
      throw new InternalDriverException();
    }

    // eslint-disable-next-line eqeqeq
    if (ecmaValue == null || !(ecmaValue instanceof Array)) {
      throw InvalidJsonFactory.ecmaArrayExpected(meta.name, ecmaValue);
    } else if (ecmaValue.length !== length) {
      throw InvalidJsonFactory.malformedEcmaArray(
        meta.name, length, ecmaValue, meta.occurs.dependingOn
      );
    }

    const positionalString: string[] = [];
    for (let i = 0; i < length; i++) {
      positionalString.push(
        this.ecmaToPositionalString(meta, ecmaValue[i], context)
      );
    }

    return positionalString.join('');
  }

  private ecmaStringToAlphabetic(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ): string {
    const { metaName, metatypeLength } = this.validateMetaType(meta);
    ecmaValue = this.getEcmaValueDefaultValue(meta, ecmaValue);

    try {
      if (this.validator.alphabeticStringValidate(meta, ecmaValue)) {
        return this.fillWithSpaces(context[metaName] = ecmaValue, metatypeLength);
      } else {
        //  the validate function return true or throw, so... if I get here is
        //  because someone broke some code
        throw new InternalDriverException();
      }
    } catch (e) {
      if (e instanceof InternalDriverException || e instanceof InvalidJsonException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }
  }

  private ecmaStringToCharacteres(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ): string {
    const { metaName, metatypeLength } = this.validateMetaType(meta);
    ecmaValue = this.getEcmaValueDefaultValue(meta, ecmaValue);

    try {
      if (this.validator.characteresStringValidate(meta, ecmaValue)) {
        return this.fillWithSpaces(context[metaName] = ecmaValue, metatypeLength);
      } else {
        //  the validate function return true or throw, so... if I get here is
        //  because someone broke some code
        throw new InternalDriverException();
      }
    } catch (e) {
      if (e instanceof InternalDriverException || e instanceof InvalidJsonException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }
  }

  private ecmaNumberToCobolNumber(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: MetaValue | undefined }
  ): string {
    const { metaName } = this.validateMetaType(meta);
    ecmaValue = this.getEcmaValueDefaultValue(meta, ecmaValue);
    this.validateMetaNumber(meta, ecmaValue);
    context[metaName] = ecmaValue;

    let decimalValue = String(ecmaValue).replace(
      EcmaToCobolService.SIGN_N_THE_INTEGER_ONES, ''
    );
    let integerValue = String(ecmaValue).replace(
      EcmaToCobolService.SIGN_N_THE_DECIMAL_ONES, ''
    );

    while (integerValue.length < meta.metatype.length)
      integerValue = `0${integerValue}`;

    if (meta.metatype.decimal !== false) {
      while (decimalValue.length < meta.metatype.decimal)
        decimalValue = `${decimalValue}0`;
    }

    if (meta.metatype.signed) {
      integerValue = this.setSignInNumeric(integerValue, ecmaValue < 0);
    }

    return `${integerValue}${decimalValue}`;
  }

  private getEcmaValueDefaultValue(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined
  ): MetaValue {
    if (ecmaValue === undefined) {
      ecmaValue = meta.metatype.default || 0;
    }

    return ecmaValue;
  }

  private validateMetaType(meta: MetaModelStructure): { metaName: string, metatypeLength: number } {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    const metaName = meta.name;
    const metatypeLength = meta.metatype.length;

    return { metaName, metatypeLength };
  }

  private validateMetaNumber(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined
  ): ecmaValue is MetaValue | undefined {
    try {
      if (!this.validator.ecmaNumberValidate(meta, ecmaValue)) {
        //  the validate function return true or throw, so... if I get here is
        //  because someone broke some code
        throw new InternalDriverException();
      }
    } catch (e) {
      if (e instanceof InternalDriverException || e instanceof InvalidJsonException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }

    return true;
  }

  private setSignInNumeric(integerString: string, isPositive: boolean): string {
    let lastChar: string | undefined = integerString[integerString.length - 1];
    if (isPositive) {
      lastChar = this.SINGNED_POSITIVE[Number(lastChar)];
    } else {
      lastChar = this.SINGNED_NEGATIVE[Number(lastChar)];
    }

    return integerString.replace(EcmaToCobolService.LAST_CHAR, lastChar);
  }

  private ecmaObjectToCobolStructure(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ): string {
    if (!meta.attributes) {
      throw new InternalDriverException();
    }

    let positionalStructure = '';
    ecmaValue = ecmaValue || {};
    if (this.validator.ecmaObjectValidate(meta, ecmaValue)) {
      for (let i = 0; i < meta.attributes.length; i++) {
        const attr = meta.attributes[i];
        if (attr.name) {
          positionalStructure += this.convertSingleRoot(attr, ecmaValue[attr.name], context);
        }
      }
    }

    return positionalStructure;
  }

  private fillWithSpaces(value: string, length: number): string {
    if (value.length > length) throw new InternalDriverException();

    while (value.length < length) value = `${value} `;

    return value;
  }
}
