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
  JvStringListToHTML, JvFormToHTML, JvFTPGrabber, JvHTMLParser, JvHTTPGrabber,
  JvMultiHTTPGrabber, JvRGBToHTML, JvRichEditToHTML, JvStrToHTML, JvMail,
  JvMailEditor, JvHTMLParserEditor, JvUrlListGrabber, JvUrlGrabbers, JvUrlListGrabberEditors;

{$R ..\resources\JvNetReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteInterNetWork,[
    TJvFTPGrabber, TJvHTTPGrabber, TJvMultiHTTPGrabber,
    TJvMail, TJvHTMLParser,
    TJvStrToHTML, TJvStringListToHTML, TJvFormToHTML,
    TJvRichEditToHTML, TJvRGBToHTML , TJvUrlListGrabber
    ]);

  RegisterPropertyEditor( TypeInfo(TJvParserInfoList),
                          TJvHTMLParser,
                          'Parser',
                          TJvHTMLParserEditor);
  RegisterPropertyEditor( TypeInfo(TJvUrlGrabberDefaultPropertiesList),
                          TJvUrlListGrabber,
                          '',
                          TJvUrlGrabberDefaultPropertiesListEditor);
  RegisterPropertyEditor( TypeInfo(TJvUrlGrabberDefaultProperties),
                          TJvUrlGrabberDefPropEdTrick,
                          '',
                          TJvUrlGrabberDefaultPropertiesEditor);

  RegisterComponentEditor(TJvMail, TJvMailEditor);
end;

end.
