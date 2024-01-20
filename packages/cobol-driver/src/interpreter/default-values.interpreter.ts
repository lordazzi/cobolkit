import { IterableString } from '@belomonte/iterable-string';
import { InvalidBookFactory } from '../business/invalid-book.factory';
import { FillableType } from '../model/fillable-type.enum';
import { MetaModelStructure } from '../model/meta-model-structure.model';

export class DefaultValuesInterpreter {

  static readonly GET_SPACES_OR_ZEROS = /^\s*(SPACE|ZERO)S?/;
  static readonly ANYTHING_AFTER_SOME_SPACE = /^\s*[^ ]+/;
  static readonly DOT_AT_THE_END = /\.$/;
  static readonly STRING_CHAR_WRAPPING = /(^"|"$)|(^'|'$)/g;

  private static instance: DefaultValuesInterpreter;

  static getInstance(): DefaultValuesInterpreter {
    if (!this.instance) {
      this.instance = new DefaultValuesInterpreter();
    }
    return this.instance;
  }

  private constructor() { }

  interpret(metaData: MetaModelStructure, stringCommand: IterableString) {
    if (stringCommand.spy(DefaultValuesInterpreter.GET_SPACES_OR_ZEROS)) {
      const fill = stringCommand.addCursor(DefaultValuesInterpreter.GET_SPACES_OR_ZEROS).trim();
      if (fill === 'ZERO' || fill === 'ZEROS' || fill === 'ZEROES') {
        metaData.metatype.fill = FillableType.ZEROS;
      } else if (fill === 'SPACE' || fill === 'SPACES') {
        metaData.metatype.fill = FillableType.SPACES;
      }
    } else {
      let defaultValue = stringCommand.addCursor(DefaultValuesInterpreter.ANYTHING_AFTER_SOME_SPACE).trim();
      if (!defaultValue) {
        throw InvalidBookFactory.invalidCommandLine(metaData.line, String(stringCommand));
      }
      defaultValue = defaultValue.replace(DefaultValuesInterpreter.DOT_AT_THE_END, '');

      if (isNaN(Number(defaultValue))) {
        metaData.metatype.default = defaultValue.replace(DefaultValuesInterpreter.STRING_CHAR_WRAPPING, '');
        metaData.metatype.fill = FillableType.SPACES;
      } else {
        metaData.metatype.default = Number(defaultValue);
        metaData.metatype.fill = FillableType.ZEROS;
      }
    }
  }
}