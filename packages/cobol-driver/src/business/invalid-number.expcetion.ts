import { ExceptionType } from './exception-type.enum';
import { Exception } from './exception.interface';

export class InvalidNumberException implements Exception {
  statusCode = 500;
  type = ExceptionType.UNKNOW;

  constructor(
    public message: string
  ) { }
}
