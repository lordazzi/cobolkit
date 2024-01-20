import { ExceptionType } from "./exception-type.enum";
import { Exception } from "./exception.interface";

export class InvalidBookException implements Exception {
  statusCode = 400;
  type = ExceptionType.COPYBOOK;

  constructor(
    public line: number,
    public message: string
  ) { }
}