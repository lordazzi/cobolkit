import { InternalDriverException } from '../business/internal-driver.exception';
import { InvalidBookException } from '../business/invalid-book.exception';
import { InvalidJsonException } from '../business/invalid-json.exception';
import { InvalidNumberException } from '../business/invalid-number.expcetion';
import { CobolDataType } from '../model/cobol-data-type.enum';
import { MetaModelStructure } from '../model/meta-model-structure.model';
import { LogType } from '../service/log-type.enum';
import { LoggerService } from './../service/logger.service';
import { EcmaValueValidator } from './ecma-value.validator';
import { NumberValidator } from './number.validator';

export class MetaModelValidator {
  private static instance: MetaModelValidator;

  static getInstance(): MetaModelValidator {
    if (!this.instance) {
      this.instance = new MetaModelValidator();
    }

    return this.instance;
  }

  private ecmaValueValidator = EcmaValueValidator.getInstance();
  private numberValidator = NumberValidator.getInstance();

  // eslint-disable-next-line @typescript-eslint/no-empty-function
  private constructor() { }

  validate(command: MetaModelStructure): true {
    LoggerService
      .getInstance()
      .log(LogType.DEBUG, ` :: Validating attribute "${command.name}"`);

    this.validateLevel(command);
    this.validateFieldLength(command);

    try {
      switch (command.metatype.type) {
        case CobolDataType.ALPHABETIC:
          return this.validateMetaDataAsAlphabetic(command);
        case CobolDataType.CHARACTERES:
          return this.validateMetaDataAsCharactere(command);
        case CobolDataType.NUMBER:
          return this.validateMetaDataAsNumeric(command);
        default:
          return true;
      }
    } catch (e) {
      if (e instanceof InvalidJsonException) {
        throw new InvalidBookException(
          command.line, `Invalid default value: ${e.message}`
        );
      } else if (e instanceof InternalDriverException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }
  }

  private validateLevel(command: MetaModelStructure): void {
    try {
      this.numberValidator.validate(command.level, {
        zeroNotAllowed: true,
        negativeNotAllowed: true,
        decimalNotAllowed: true,
        maxNumber: 77
      });
    } catch (e) {
      if (e instanceof InvalidNumberException) {
        throw new InvalidBookException(command.line,
          `Invalid level: ${e.message}, in file ${command.bookName}`
        );
      } else if (e instanceof InternalDriverException) {
        throw e;
      } else {
        throw new InternalDriverException(e);
      }
    }
  }

  private validateFieldLength(command: MetaModelStructure): void {
    if (command.metatype.length !== false) {
      try {
        this.numberValidator.validate(command.metatype.length, {
          zeroNotAllowed: true,
          negativeNotAllowed: true,
          decimalNotAllowed: true
        });
      } catch (e) {
        if (e instanceof InvalidNumberException) {
          throw new InvalidBookException(command.line,
            `Invalid number given as field length: ${e.message}`
          );
        } else if (e instanceof InternalDriverException) {
          throw e;
        } else {
          throw new InternalDriverException(e);
        }
      }
    }
  }

  private validateMetaDataAsAlphabetic(command: MetaModelStructure): true {
    if (command.metatype.default !== undefined) {
      this.ecmaValueValidator.alphabeticStringValidate(
        command, command.metatype.default
      );
    }

    return true;
  }

  private validateMetaDataAsCharactere(command: MetaModelStructure): true {
    if (command.metatype.default !== undefined) {
      this.ecmaValueValidator.characteresStringValidate(
        command, command.metatype.default
      );
    }

    return true;
  }

  private validateMetaDataAsNumeric(command: MetaModelStructure): true {
    if (command.metatype.default !== undefined) {
      this.ecmaValueValidator.ecmaNumberValidate(
        command, command.metatype.default
      );
    }

    return true;
  }
}
