import { IterableString } from '@belomonte/iterable-string';
import { InternalDriverException } from '../business/internal-driver.exception';
import { InvalidBookFactory } from '../business/invalid-book.factory';
import { MetaModelStructureOccurs } from '../model/meta-model-structure-occurs.model';
import { MetaModelStructure } from '../model/meta-model-structure.model';
import { ReservedWordType } from './../model/reserved-word.type';

export class OccursInterpreter {

  static NEXT_NUMBER = /^\s*\d+/;
  static NEXT_WORD = /^\s*[^ ]+/;
  static OCCURS_DOMAIN_KEYWORDS = /^\s*(INDEXED BY|DEPENDING ON|TO|TIMES)/;

  private static instance: OccursInterpreter;

  static getInstance(): OccursInterpreter {
    if (!this.instance) {
      this.instance = new OccursInterpreter();
    }
    return this.instance;
  }

  private constructor() { }

  interpret(metaData: MetaModelStructure, stringCommand: IterableString): void {
    const length = Number(stringCommand.addCursor(OccursInterpreter.NEXT_NUMBER).trim());
    let occursDomainCommand: ReservedWordType;
    metaData.occurs = new MetaModelStructureOccurs();
    metaData.occurs.minLength = length;
    metaData.occurs.maxLength = length;

    while ((occursDomainCommand = stringCommand.addCursor(OccursInterpreter.OCCURS_DOMAIN_KEYWORDS).trim() as ReservedWordType)) {
      if (occursDomainCommand === 'TO') {
        metaData.occurs.maxLength = Number(stringCommand.addCursor(OccursInterpreter.NEXT_NUMBER).trim());
      } else if (occursDomainCommand === 'DEPENDING ON') {
        metaData.occurs.dependingOn = stringCommand.addCursor(OccursInterpreter.NEXT_WORD);
      } else if (occursDomainCommand === 'INDEXED BY' || occursDomainCommand === 'TIMES') {
        //  INDEXED BY is not used in integration and I don't do nothing with the TIMES keyword
        continue;
      } else {
        throw new InternalDriverException(
          `This error may be caused by editing the regex OCCURS_DOMAIN_KEYWORDS.`
        );
      }
    }

    if (metaData.occurs.minLength != metaData.occurs.maxLength && !metaData.occurs.dependingOn) {
      throw InvalidBookFactory.occursWithLengthRangeButNoDependingOn(metaData);
    }
  }
}