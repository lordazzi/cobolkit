import { CobolDataType } from './cobol-data-type.enum';
import { FillableType } from './fillable-type.enum';

/**
 * This class represents a cobol datatype with it characteristics
 */
export class MetaModelStructureType {

    /**
     * This says what this datatype represents
     */
    type = CobolDataType.OBJECT;

    /**
     * This says if binary will used stead string
     */
    binary = false;

    /**
     * Represents the length of an occurs or a string length 
     */
    length: number | false = false;

    /**
     * This attribute show which value the book must initialize
     */
    default?: number | string;

    /**
     * This attribute show which the default character to be set when there is
     * no value in the field: zeros or spaces
     */
    fill?: FillableType;

    /**
     * If the number has decimals, this attribute show how many decimals digits are on it,
     * otherwise this will be setted as false
     */
    decimal: number | false = false;

    /**
     * If the datatype is a number, this can be signed to identify if it is a negative
     * number or positive
     */
    signed = false;
}
