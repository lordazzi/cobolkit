import { ExceptionType } from './exception-type.enum';

export interface Exception {
  type: ExceptionType;
  statusCode: number;
  message: string;
}