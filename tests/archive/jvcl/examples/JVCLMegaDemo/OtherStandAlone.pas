unit OtherStandAlone;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvLabel, JvHotLink, ComCtrls;

type
  TOtherMainForm = class(TForm)
    JvHotLink1: TJvHotLink;
    Label1: TLabel;
    JvHotLink2: TJvHotLink;
    JvHotLink4: TJvHotLink;
    JvHotLink3: TJvHotLink;
    JvHotLink5: TJvHotLink;
    JvHotLink6: TJvHotLink;
    JvHotLink7: TJvHotLink;
    JvHotLink8: TJvHotLink;
    JvHotLink9: TJvHotLink;
    JvHotLink10: TJvHotLink;
    Label2: TLabel;
    JvHotLink11: TJvHotLink;
    JvHotLink12: TJvHotLink;
    JvHotLink13: TJvHotLink;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    JvHotLink14: TJvHotLink;
    JvHotLink15: TJvHotLink;
    JvHotLink16: TJvHotLink;
    Label6: TLabel;
    JvHotLink17: TJvHotLink;
    JvHotLink18: TJvHotLink;
    JvHotLink19: TJvHotLink;
    JvHotLink20: TJvHotLink;
    Label7: TLabel;
    JvHotLink21: TJvHotLink;
    StatusBar: TStatusBar;
    procedure JvHotLinkMouseEnter(Sender: TObject);
    procedure JvHotLinkMouseLeave(Sender: TObject);
    procedure JvHotLinkClick(Sender: TObject);
    procedure JvHotLinkBuildJVCLClick(Sender: TObject);
  end;

var
  OtherMainForm: TOtherMainForm;

implementation

uses
  ShellAPI, JclStrings;

{$R *.dfm}

var
  hotLinkCaption : string;

procedure TOtherMainForm.JvHotLinkMouseEnter(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvHotLink;
begin
 hotLnk := sender as TJvHotLink;
 hotLinkCaption := hotLnk.caption;

 fileName := ExtractFilePath(Application.ExeName) + (hotLnk).Url;

 if not fileExists(fileName) then
 begin
   hotLnk.font.Color := clred;
   hotLnk.Caption := 'file not found';
   StatusBar.SimpleText := 'file ''' + fileName + ''' not found';;
 end
 else
 begin
   hotLnk.font.Color := clGreen;
   hotLnk.Caption := 'click to start';
   StatusBar.SimpleText := 'click to start file ''' + fileName + '''';
 end;
end;

procedure TOtherMainForm.JvHotLinkMouseLeave(Sender: TObject);
var
  hotLnk : TJvHotLink;
begin
  hotLnk := sender as TJvHotLink;
  hotLnk.caption := hotLinkCaption;
  hotLnk.font.color := clBlue;
end;

procedure TOtherMainForm.JvHotLinkClick(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvHotLink;
begin
 hotLnk := sender as TJvHotLink;
 hotLnk.caption := hotLinkCaption;

 fileName := ExtractFilePath(Application.ExeName) + (hotLnk).Url;
 if fileExists(fileName) then
   ShellExecute(0, Nil, pChar (fileName), Nil, Nil, SW_HIDE);

end;

procedure TOtherMainForm.JvHotLinkBuildJVCLClick(Sender: TObject);
var
  fileName : String;
  hotLnk : TJvHotLink;
begin
 hotLnk := sender as TJvHotLink;
 hotLinkCaption := hotLnk.caption;

 fileName := ExtractFilePath(Application.ExeName);
 StrReplace(fileName, '\bin', '', [rfIgnoreCase]);

 fileName := fileName + 'examples\CompileExamples.bat';
 if fileExists(fileName) then
   ShellExecute(0, Nil, pChar (fileName), Nil, Nil, SW_NORMAL);


end;

end.
