import { ExceptionType } from './exception-type.enum';
import { Exception } from './exception.interface';

export class InternalDriverException implements Exception {
  readonly statusCode = 500;
  message = '';
  type = ExceptionType.UNKNOW;
  cause?: Error | Exception;

  static isException(error: unknown): error is Exception {
    return !!(error instanceof Object && (error as Exception).message && (error as Exception).statusCode);
  }

  constructor(
    msg?: string | Error | Exception | unknown
  ) {
    if (msg instanceof Error || InternalDriverException.isException(msg)) {
      this.cause = msg;
    } else if (msg !== undefined) {
      this.message = `Driver internal exception: some maintance in the drive caused a crash. ${msg}`;
    } else {
      this.message = 'Driver internal exception: some maintance in the drive caused a crash.';
    }
  }
}