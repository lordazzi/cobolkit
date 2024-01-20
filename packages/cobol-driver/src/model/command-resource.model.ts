/**
 * Value Object that represent a read command in a copybook content
 */
export class CommandResource {

    /**
     * Represent the read command, represented in an array, splitted by \n
     */
    command: string[] = [];

    /**
     * Line where the command starts
     */
    commandLineNumber = 0;

    /**
     * If there is any comment above the command, it comes in this property
     */
    blockCommentAbove?: string;

    /**
     * If there is any comment below the command, it comes in this property
     */
    blockCommentBelow?: string;

    /**
     * The comment write in the six first columns.
     * A command can use more the one line to be written, then each command
     * can have the amount of ancestor comments according to its number of
     * lines
     */
    ancestorComment?: string[];
}
