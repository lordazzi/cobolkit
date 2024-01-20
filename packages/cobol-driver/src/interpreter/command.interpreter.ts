import { IterableString } from '@belomonte/iterable-string';
import { InvalidBookFactory } from '../business/invalid-book.factory';
import { MetaModelStructure } from '../model/meta-model-structure.model';
import { DefaultValuesInterpreter } from './default-values.interpreter';
import { OccursInterpreter } from './occurs.interpreter';
import { PicInterpreter } from './pic.interpreter';

export class CommandInterpreter {

  static readonly EVERY_THING_UNTIL_THE_FIRST_SPACE = /^\s*[^ ]+/;

  private static instance: CommandInterpreter;

  static getInstance() {
    if (!this.instance) {
      this.instance = new CommandInterpreter();
    }

    return this.instance;
  }

  private constructor() { }

  interpretBinary(metaData: MetaModelStructure, stringCommand: IterableString): void {
    metaData.metatype.binary = true;
  }

  interpretPic(metaData: MetaModelStructure, stringCommand: IterableString): void {
    const picCommand = stringCommand.addCursor(
      CommandInterpreter.EVERY_THING_UNTIL_THE_FIRST_SPACE
    ).trim();

    if (!picCommand) {
      throw InvalidBookFactory.invalidPic(metaData.line, picCommand);
    }

    PicInterpreter.getInstance().interpret(metaData, picCommand);
  }

  interpretValues(metaData: MetaModelStructure, stringCommand: IterableString): void {
    DefaultValuesInterpreter.getInstance().interpret(metaData, stringCommand);
  }

  interpretOccurs(metaData: MetaModelStructure, stringCommand: IterableString): void {
    OccursInterpreter.getInstance().interpret(metaData, stringCommand);
  }
}