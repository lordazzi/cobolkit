import { Injectable } from '@angular/core';
import { CobolDataType, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';
import { FieldError } from './field-error.interface';

@Injectable()
export class DynamicFieldService {

  private readonly WRITTEN_NUMERIC_VALUE = /^-?\d+(,\d+)?$/;
  private readonly WRITTEN_DECIMAL_WITH_DOT = /^-?\d+\.\d+?$/;
  private readonly WRITTEN_ALPHABETIC_VALUE = /^[A-Z]*$/i;

  updateValue(entryData: string, structure?: MetaModelStructure): string | number {
    let validValue: string | number = '';

    if (!structure) {
      throw { message: 'O campo não encontrou o objeto que rege as informações de tratamento do dado' };
    }

    if (structure.metatype.type === CobolDataType.ALPHABETIC) {
      validValue = this.castToValidAlphabetic(entryData, structure);
    } else if (structure.metatype.type === CobolDataType.CHARACTERES) {
      validValue = this.castToValidCharacter(entryData, structure);
    } else if (structure.metatype.type === CobolDataType.NUMBER) {
      validValue = this.castToValidNumeric(entryData, structure);
    } else {
      throw { message: 'Tipo de dado do campo não está mepeado' } as FieldError;
    }

    return validValue;
  }

  castToValidNumeric(data: string, structure: MetaModelStructure): number {
    if (!data) {
      return 0;
    }

    if (this.WRITTEN_DECIMAL_WITH_DOT.test(data)) {
      throw { message: 'Utilize vírgula para valores decimais' };
    }

    if (!this.WRITTEN_NUMERIC_VALUE.test(data)) {
      throw { message: `O valor "${data}" entregue não é numerico` };
    }

    if (data === '-') {
      data = '0';
    }

    data = data.replace(/,$/, '').replace(/,/, '.');

    return this.validateNumeric(Number(data), structure);
  }

  private validateNumeric(numeric: number, structure: MetaModelStructure): number {
    if (!structure.metatype.decimal && numeric % 1 !== 0) {
      throw { message: 'O campo não comporta valores decimais' };
    }

    if (!structure.metatype.signed && numeric < 0) {
      throw { message: 'Este campo não comporta valores negativos' };
    }

    this.validateNumericLength(numeric, structure);

    return numeric;
  }

  private validateNumericLength(numeric: number, structure: MetaModelStructure): void {
    let numericAsString = String(numeric);
    numericAsString = numericAsString.replace(/^-/, '');
    const numericPieces = numericAsString.split('.');
    const integerPiece = numericPieces[0];
    const decimalPiece = numericPieces[1];

    if (integerPiece && integerPiece.length > Number(structure.metatype.length)) {
      throw { message: `Valor inteiro passa do tamanho de ${structure.metatype.length} caracteres` };
    }

    if (decimalPiece && decimalPiece.length > Number(structure.metatype.decimal)) {
      throw { message: `Valor decimal passa do tamanho de ${structure.metatype.decimal} caracteres` };
    }
  }

  castToValidAlphabetic(data: string, structure: MetaModelStructure): string {
    if (!this.WRITTEN_ALPHABETIC_VALUE.test(data)) {
      throw { message: 'O campo alphabetic comporta somente letras' };
    }

    return this.castToValidCharacter(data, structure);
  }

  castToValidCharacter(data: string, structure: MetaModelStructure): string {
    if (structure.metatype.length !== false && data.length > structure.metatype.length) {
      throw { message: 'O campo atingiu o tamanho máximo de caracteres' };
    }

    return data;
  }
}
