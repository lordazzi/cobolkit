import { ExceptionType } from "./exception-type.enum";
import { Exception } from "./exception.interface";

export class InvalidPositionalStringException implements Exception {
  statusCode = 422;
  type = ExceptionType.POSITIONAL_STRING;

  constructor(
    public fromPos: number,
    public toPos: number,
    public message: string
  ) { }
}