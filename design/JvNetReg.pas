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
  JvTypes,
  JvStringListToHtml, JvFormToHtml, JvFtpGrabber, JvHtmlParser, JvHttpGrabber,
  JvMultiHttpGrabber, JvRgbToHtml, JvRichEditToHtml, JvStrToHtml, JvMail,
  JvMailEditor, JvHTMLParserEditor;

{$R ..\resources\JvNetReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Network',[
    TJvStringListToHtml, TJvFormToHtml, TJvHtmlParser, TJvRgbToHtml,
    TJvFtpGrabber, TJvHttpGrabber, TJvMultiHttpGrabber,
    TJvRichEditToHtml, TJvStrToHtml, TJvMail
    ]);

  RegisterPropertyEditor(TypeInfo(TJvParserInfoList), TJvHtmlParser, 'Parser', TJvHtmlParserEditor);
  
  RegisterComponentEditor(TJvMail, TJvMailEditor);
end;

end.
