import { CobolToEcmaService, CopybookInterpreter, EcmaToCobolService, MetaModel, MetaModelStructure, MetaValue } from '@cobolkit-reborn/cobol-driver';
import { Controller, Post, Req, Res } from '@nestjs/common';
import { Request, Response } from 'express';


@Controller('cobolkit')
export class AppController {

  @Post('copybook-to-metadata')
  castCopybookToMetadata(@Req() req: Request, @Res() res: Response): Response<MetaModelStructure[]> {
    const requestContent: { name: string; content: string } = req.body;

    const metaData = CopybookInterpreter
      .getInstance()
      .interpret(requestContent.content, requestContent.name);

    return res.send(metaData);
  }

  @Post('ecma-to-cobol')
  castEcmaToCobolString(@Req() req: Request, @Res() res: Response): Response<string> {
    const requestContent: { metaData: MetaModelStructure[]; metaModel: MetaModel } = req.body;

    const positionalString = EcmaToCobolService
      .getInstance()
      .convert(requestContent.metaData, requestContent.metaModel);

    return res.send(JSON.stringify(positionalString));
  }

  @Post('cobol-to-ecma')
  castCobolStringToEcma(@Req() req: Request, @Res() res: Response): Response<MetaValue> {
    const requestContent: { metaData: MetaModelStructure[]; positionalString?: string } = req.body;
    let metaValue: MetaValue | null = null;

    if (requestContent.positionalString) {
      metaValue = CobolToEcmaService
        .getInstance()
        .convert(requestContent.metaData, requestContent.positionalString);
    } else {
      metaValue = CobolToEcmaService
        .getInstance()
        .convert(requestContent.metaData);
    }

    return res.send(metaValue);
  }
}
