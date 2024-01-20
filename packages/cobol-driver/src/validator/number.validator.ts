/* eslint-disable complexity */
import { InvalidNumberException } from '../business/invalid-number.expcetion';

export class NumberValidator {
  private static instance: NumberValidator;

  static getInstance(): NumberValidator {
    if (!this.instance) {
      this.instance = new NumberValidator();
    }

    return this.instance;
  }

  private constructor() { }

  validate(
    validMe: unknown, config: {
      zeroNotAllowed?: boolean,
      negativeNotAllowed?: boolean,
      decimalNotAllowed?: boolean,
      maxNumber?: number,
      minNumber?: number
    } = {}
  ): validMe is number {
    let validMeAsText = String(validMe);
    if (validMe instanceof Object) {
      validMeAsText = JSON.stringify(validMe);
    }

    if (Number.isNaN(validMe) || !Number.isFinite(validMe) || validMe === null) {
      throw new InvalidNumberException(`invalid expression given as number: value "${validMeAsText}" with the type "${typeof validMe}"`);
    } else if (typeof validMe !== 'number') {
      throw new InvalidNumberException('isn\'t numeric');
    } else if (!Number.isSafeInteger(validMe) && validMe % 1 === 0) {
      throw new InvalidNumberException(`${validMeAsText} isn't a safe ecma number`);
    } else if (config.negativeNotAllowed === true && validMe < 0) {
      throw new InvalidNumberException(`negative number is not allowed. ${validMeAsText} was given`);
    } else if (config.zeroNotAllowed === true && validMe === 0) {
      throw new InvalidNumberException('zero is not allowed');
    } else if (config.decimalNotAllowed === true && validMe % 1 !== 0) {
      throw new InvalidNumberException(`decimal number is not allowed. ${validMeAsText} was given`);
    } else if (config.maxNumber !== undefined && config.maxNumber < validMe) {
      throw new InvalidNumberException(
        `number can't be major then ${config.maxNumber}. ${validMeAsText} was given`
      );
    } else if (config.minNumber !== undefined && config.minNumber > validMe) {
      throw new InvalidNumberException(
        `number can't be minor then ${config.maxNumber}. ${validMeAsText} was given`
      );
    }

    return true;
  }
}
