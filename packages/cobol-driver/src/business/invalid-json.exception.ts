import { ExceptionType } from "./exception-type.enum";
import { Exception } from "./exception.interface";

export class InvalidJsonException implements Exception {
  statusCode = 422;
  type = ExceptionType.META_DATA;

  public message: string;

  constructor(
    fieldName: string,
    msg?: string
  ) {
    if (!msg) {
      this.message = `Ecma conversion to positional string failed at field "${fieldName}".`;
    } else {
      this.message =
        `Ecma conversion to positional string failed at field "${fieldName}": ${msg}.`;
    }
  }
}