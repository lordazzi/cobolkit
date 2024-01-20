import { LogType } from "./log-type.enum";

export class LoggerService {

    static logLevel = null;

    private static instance: LoggerService;
    static getInstance(): LoggerService {
        if (!this.instance) {
            this.instance = new LoggerService();
        }

        return this.instance;
    }

    log(logType: LogType, message: string): void {
        if (LoggerService.logLevel)
            switch (logType) {
                case LogType.INFORMATIVE:
                case LogType.DEBUG:
                    console.info.apply(console, [logType, message]);
                    break;
                case LogType.ERROR:
                    console.error.apply(console, [logType, message]);
                    break;
            }
    }
}