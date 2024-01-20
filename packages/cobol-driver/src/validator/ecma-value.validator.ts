/* eslint-disable @typescript-eslint/no-empty-function */
import { InvalidJsonFactory } from '../business/invalid-json.factory';
import { InvalidNumberException } from '../business/invalid-number.expcetion';
import { MetaModelStructure } from '../model/meta-model-structure.model';
import { MetaModel } from '../model/meta-model.type';
import { MetaValue } from '../model/meta-value.type';
import { InternalDriverException } from './../business/internal-driver.exception';
import { InvalidJsonException } from './../business/invalid-json.exception';
import { NumberValidator } from './number.validator';

export class EcmaValueValidator {
  private static instance: EcmaValueValidator;

  static ALPHABETIC_COVARAGE = /^[a-z ]*$/i;
  static SIGN_N_THE_INTEGER_ONES = /^-?\d*(\.|$)/;
  static SIGN_N_THE_DECIMAL_ONES = /^-?|\.\d*$/g;

  static getInstance(): EcmaValueValidator {
    if (!this.instance) {
      this.instance = new EcmaValueValidator();
    }

    return this.instance;
  }

  private numberValidator = NumberValidator.getInstance();

  private constructor() { }

  alphabeticStringValidate(
    meta: MetaModelStructure, ecmaValue: MetaValue
  ): ecmaValue is string {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    if (typeof ecmaValue !== 'string') {
      throw InvalidJsonFactory.ecmaStringExpected(meta.name, ecmaValue);
    } else if (!ecmaValue.match(EcmaValueValidator.ALPHABETIC_COVARAGE)) {
      throw InvalidJsonFactory.invalidAlphabeticValue(meta.name, ecmaValue);
    } else if (ecmaValue.length > meta.metatype.length) {
      throw InvalidJsonFactory.invalidStringLength(
        meta.name, ecmaValue, meta.metatype.length
      );
    }

    return true;
  }

  characteresStringValidate(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined
  ): ecmaValue is string {
    if (!meta.name || !meta.metatype.length) {
      throw new InternalDriverException();
    }

    if (typeof ecmaValue !== 'string') {
      throw InvalidJsonFactory.ecmaStringExpected(meta.name, ecmaValue);
    } else if (ecmaValue.length > meta.metatype.length) {
      throw InvalidJsonFactory.invalidStringLength(
        meta.name, ecmaValue, meta.metatype.length
      );
    }

    return true;
  }

  // eslint-disable-next-line complexity
  ecmaNumberValidate(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined
  ): ecmaValue is number {
    if (!meta.name) {
      throw new InternalDriverException();
    }

    const config: {
      negativeNotAllowed?: boolean,
      decimalNotAllowed?: boolean
    } = {};
    config.negativeNotAllowed = !meta.metatype.signed;
    config.decimalNotAllowed = !meta.metatype.decimal;

    try {
      if (this.numberValidator.validate(ecmaValue, config)) {
        const integerLength = String(ecmaValue).replace(
          EcmaValueValidator.SIGN_N_THE_DECIMAL_ONES, ''
        ).length;

        if (integerLength > Number(meta.metatype.length)) {
          throw new InvalidJsonException(
            meta.name, `number overflow, max of ${meta.metatype.decimal
            } length accept, but a number of length ${integerLength} was given`
          );
        }

        const decimalLength = String(ecmaValue).replace(
          EcmaValueValidator.SIGN_N_THE_INTEGER_ONES, ''
        ).length;

        if (meta.metatype.decimal !== false && decimalLength > meta.metatype.decimal) {
          throw new InvalidJsonException(
            meta.name, `decimal overflow, max of ${meta.metatype.decimal
            } decimal length accept, but a number of length ${decimalLength
          } was given`
          );
        }
      } else {
        //  the validator returns true or trow so,
        //  enter in this else is impossible
        throw new InternalDriverException();
      }
    } catch (e) {
      if (e instanceof InvalidNumberException) {
        throw new InvalidJsonException(
          meta.name, e.message
        );
      } else if (e instanceof InternalDriverException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }

    return true;
  }

  ecmaObjectValidate(
    meta: MetaModelStructure, ecmaValue: MetaValue
  ): ecmaValue is MetaModel {
    if (!meta.name || !ecmaValue) {
      throw new InternalDriverException();
    }

    if (ecmaValue.constructor === Object) {
      return true;
    } else {
      throw new InvalidJsonException(meta.name, `is not an instance of Object`);
    }
  }

  ecmaArrayValidate(
    meta: MetaModelStructure, ecmaValue: MetaValue | undefined,
    context: { [prop: string]: number | string | undefined }
  ) { }
}