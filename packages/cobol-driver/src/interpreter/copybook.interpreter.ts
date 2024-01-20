import { IterableString } from '@belomonte/iterable-string';
import { InvalidBookFactory } from '../business/invalid-book.factory';
import { CommandResource } from '../model/command-resource.model';
import { ReservedWordType } from '../model/reserved-word.type';
import { MetaModelValidator } from '../validator/meta-model.validator';
import { CobolDataType } from './../model/cobol-data-type.enum';
import { MetaModelStructure } from './../model/meta-model-structure.model';
import { CommandInterpreter } from './command.interpreter';

export class CopybookInterpreter {

  static readonly ALL_RESET_CURSOR_CHAR = /\r/g;
  static readonly ALL_NEW_LINE_CHAR = /n/g;
  static readonly WHITE_SPACE_THAT_CHEAT_THE_INTERPRETER = /(\n[ ]{1,5}\n|\n\s*\n)/g;
  static readonly END_OF_COBOL_COMMAND = /^.*(\.\n|\n|$)/;
  static readonly CPY_END = /\.\s*$/;
  static readonly SPACES_UNTIL_THE_FIRST_NUMBER_GROUP = /^\s*\d+\s/;
  static readonly SPACES_THEN_EVERY_CHAR_TO_THE_FIRST_SPACE_OR_DOT = /^\s*[^ \.]+[\. ]/;
  static readonly THE_DOT_IN_THE_END = /\.$/;
  static readonly COBOL_RESERVED_WORD = /^\s*(PIC|PICTURE|VALUE|VALUES|OCCURS|TIMES|DEPENDING ON|TO|INDEXED BY|BINARY|REDEFINES|USAGE|COMP|COMPUTATIONAL|COMP\-1|COMPUTATIONAL\-1|COMP\-2|COMPUTATIONAL\-2)/;
  static readonly COMMENT_BLOCK = /^(((\n|\r)*(.{6}\*[^\n]*(\n|$)))*)/;
  static readonly NEXT_SIX_CHARS_EXCEPT_NEW_LINE = /^[^\n]{0,6}/;

  private static instance: CopybookInterpreter;

  static getInstance(): CopybookInterpreter {
    if (!this.instance) {
      this.instance = new CopybookInterpreter();
    }
    return this.instance;
  }

  private constructor() { }

  interpret(copybookContent: string, name: string): MetaModelStructure[] {
    debugger;
    const unorderedCopybookFields = new Array<MetaModelStructure>();

    this.readCopybook(copybookContent, resource => {
      const metaData = this.interpretLine(resource, name);
      if (metaData) {
        unorderedCopybookFields.push(metaData);
      }
    });

    return this.structureFieldsByLevel(
      unorderedCopybookFields
    );
  }

  private interpretLine(resource: CommandResource, bookName: string): MetaModelStructure | null {
    if (!resource.command) {
      return null;
    }

    //  join multiline commands into a single line
    const commandText = resource.command.join(' ').trim();
    if (!commandText) {
      return null;
    }

    const interpreter = CommandInterpreter.getInstance();
    const iterableCommand = new IterableString(commandText);
    const commandLevel = Number(iterableCommand.addCursor(
      CopybookInterpreter.SPACES_UNTIL_THE_FIRST_NUMBER_GROUP
    ).trim());

    const command = new MetaModelStructure(
      resource.commandLineNumber, commandLevel
    );
    let reservedWordStr: ReservedWordType | false;
    command.bookName = bookName;
    command.name = iterableCommand.addCursor(
      CopybookInterpreter.SPACES_THEN_EVERY_CHAR_TO_THE_FIRST_SPACE_OR_DOT
    ).trim();
    command.name = command.name.replace(CopybookInterpreter.THE_DOT_IN_THE_END, '');

    while ((reservedWordStr = this.nextSentenceIsReservedWord(iterableCommand))) {
      switch (reservedWordStr) {
        case 'PIC':
        case 'PICTURE':
          interpreter.interpretPic(command, iterableCommand);
          break;
        case 'BINARY':
          interpreter.interpretBinary(command, iterableCommand);
          break;
        case 'VALUE':
        case 'VALUES':
          interpreter.interpretValues(command, iterableCommand);
          break;
        case 'OCCURS':
          interpreter.interpretOccurs(command, iterableCommand);
          break;
        default:
          throw InvalidBookFactory.unsupportedCommand(
            command.line, reservedWordStr, String(iterableCommand)
          );
      }
    }

    MetaModelValidator.getInstance().validate(command);

    return command;
  }

  private readCopybook(
    cpyContent: string, calle: (resource: CommandResource) => void
  ): void {
    cpyContent = cpyContent.replace(CopybookInterpreter.ALL_RESET_CURSOR_CHAR, '');
    cpyContent = cpyContent.replace(CopybookInterpreter.WHITE_SPACE_THAT_CHEAT_THE_INTERPRETER, '\n\n');

    let command = [];
    let ancestorComment = [];
    let content = new IterableString(cpyContent);
    let blockCommentAbove = this.getCommentBlockIfExists(content);
    let blockCommentBelow = '';
    let commandLineNumber = 1;
    //	number of lines that will be add in the command line number only
    //	in the end of execution, to the next line receives it
    let aditionalLines = 0;

    while (!content.end()) {
      let isTheEnd = false;

      //  getting each new line in comment block to increase the current line number
      let linesOfCommentBlock = blockCommentAbove.match(CopybookInterpreter.ALL_NEW_LINE_CHAR);
      commandLineNumber += linesOfCommentBlock && linesOfCommentBlock.length || 0;

      while (!isTheEnd) {
        //	first six columns
        let ac = content.addCursor(CopybookInterpreter.NEXT_SIX_CHARS_EXCEPT_NEW_LINE);
        if (content.spy() === '\n') {
          content.addCursor(1);
          continue;
        }

        // I'll trim just if there is no value in the column
        ac = ac.trim() ? ac : '';

        //	go to next dot with new line or new line or the end
        let match = content.addCursor(CopybookInterpreter.END_OF_COBOL_COMMAND);

        if (!match) {
          isTheEnd = true;
          match = String(content);
        } else if (match.match(CopybookInterpreter.CPY_END)) {
          isTheEnd = true;
        }

        ancestorComment.push(ac);

        if (match) command.push(match);

        aditionalLines++;
      }

      blockCommentBelow = this.getCommentBlockIfExists(content);
      let resource = new CommandResource();

      //	create resource object
      resource.command = command;
      resource.commandLineNumber = commandLineNumber;
      resource.blockCommentAbove = blockCommentAbove;
      resource.blockCommentBelow = blockCommentBelow;
      resource.ancestorComment = ancestorComment;

      calle.apply(calle, [resource]);

      //	reset variables to the next read
      blockCommentAbove = blockCommentBelow;
      commandLineNumber += aditionalLines;
      aditionalLines = 0;
      command = [];
      ancestorComment = [];
    }
  }

  private nextSentenceIsReservedWord(iterableCommand: IterableString): ReservedWordType | false {
    const regex = CopybookInterpreter.COBOL_RESERVED_WORD;

    return iterableCommand.addCursor(regex).trim() as ReservedWordType || false;
  }

  private getCommentBlockIfExists(iterableString: IterableString): string {
    return iterableString.addCursor(CopybookInterpreter.COMMENT_BLOCK);
  }

  private structureFieldsByLevel(unorderedCopybookFields: MetaModelStructure[]): MetaModelStructure[] {
    //  the last entry in the array is the last object that will act as root
    //  to the next field
    const currentHierarch = new Array<MetaModelStructure>();
    const rootOnes = new Array<MetaModelStructure>();
    let lastAttachedObject: MetaModelStructure;

    unorderedCopybookFields.forEach(cpyField => {
      if (lastAttachedObject) {
        while (lastAttachedObject.level >= cpyField.level) {
          let objectToRollback = currentHierarch.pop();
          if (objectToRollback) {
            lastAttachedObject = objectToRollback;
            if (lastAttachedObject.level < cpyField.level) {
              currentHierarch.push(lastAttachedObject);
            }
          } else {
            rootOnes.push(cpyField);
            break;
          }
        }

        if (lastAttachedObject.level < cpyField.level) {
          if (!lastAttachedObject.attributes) {
            lastAttachedObject.attributes = [];
          }

          lastAttachedObject.attributes.push(cpyField);
        }
      } else {
        rootOnes.push(cpyField);
      }

      if (cpyField.metatype.type === CobolDataType.OBJECT) {
        if (
          lastAttachedObject && lastAttachedObject.attributes &&
          lastAttachedObject.attributes.length === 0
        ) {
          throw InvalidBookFactory.noPropertiesObject(
            lastAttachedObject.line
          );
        }

        currentHierarch.push(lastAttachedObject = cpyField);
      }
    });

    //  if there is no root neither no argument, we have a problem in the book.
    if (rootOnes && rootOnes.length) {
      return rootOnes;
    } else if (unorderedCopybookFields[0]) {
      //  if there is no object type field, no root field will be generated, that means
      //  that the book has only one text or number attribute, we can get this attribute
      //  in the array that comes as argument in the method
      return [unorderedCopybookFields[0]];
    } else {
      throw InvalidBookFactory.invalidBook();
    }
  }
}