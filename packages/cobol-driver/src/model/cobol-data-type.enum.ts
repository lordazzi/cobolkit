/**
 * This sounds weird, but this enum represents the types of the cobol data type
 */
export enum CobolDataType {
    OBJECT = 'object',
    NUMBER = 'number',
    ALPHABETIC = 'alphabetic',
    CHARACTERES = 'character',

    //  will be specified as filler type every field without name, ignoring it PICTURE
    FILLER = 'filler'
}