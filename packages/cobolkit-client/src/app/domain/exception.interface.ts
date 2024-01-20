export interface Exception {
    code: 400 | 422 | 500;
    msg: string;
    cause?: string;
    line?: number;
    fromPos?: number;
    toPos?: number;
}
