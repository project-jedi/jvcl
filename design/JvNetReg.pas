{$I JVCL.INC}

unit JvNetReg;

interface

procedure Register;

implementation
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvConsts,
  JvStringListToHtml, JvFormToHtml, JvFtpGrabber, JvHtmlParser, JvHttpGrabber,
  JvMultiHttpGrabber, JvRgbToHtml, JvRichEditToHtml, JvStrToHtml, JvMail,
  JvMailEditor, JvHTMLParserEditor;

{$R ..\resources\JvNetReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteInterNetWork,[
    TJvFtpGrabber, TJvHttpGrabber, TJvMultiHttpGrabber,
    TJvMail, TJvHtmlParser,
    TJvStrToHtml, TJvStringListToHtml, TJvFormToHtml,
    TJvRichEditToHtml, TJvRgbToHtml
    ]);

  RegisterPropertyEditor(TypeInfo(TJvParserInfoList), TJvHtmlParser, 'Parser', TJvHtmlParserEditor);
  
  RegisterComponentEditor(TJvMail, TJvMailEditor);
end;

end.
