{$I JVCL.INC}

unit JvNetReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf, DesignEditors, JvTypes, 
  JvStringListToHtml, JvFormToHtml, JvFtpGrabber, JvHtmlParser, JvHttpGrabber,
  JvMultiHttpGrabber, JvRgbToHtml, JvRichEditToHtml, JvStrToHtml, JvMail,
  JvMailEditor, JvHTMLParserEditor;

{.$R ..\resources\JvNetReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Network',[]);

  RegisterPropertyEditor(TypeInfo(TJvParserInfoList), TJvHtmlParser, 'Parser', TJvHtmlParserEditor);
  
  RegisterComponentEditor(TJvMail, TJvMailEditor);
end;

end.
