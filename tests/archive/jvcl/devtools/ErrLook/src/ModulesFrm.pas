unit ModulesFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ExtCtrls;

type
  TfrmModules = class(TForm)
    edModuleName: TEdit;
    btnBrowse: TButton;
    btnAdd: TButton;
    btnRemove: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    alModules: TActionList;
    acBrowse: TAction;
    acAdd: TAction;
    acRemove: TAction;
    lbModules: TListBox;
    acHelp: TAction;
    Label1: TLabel;
    procedure alModulesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acBrowseExecute(Sender: TObject);
    procedure acRemoveExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
    procedure SetupHelp;
    function GetModules: TStrings;
    procedure SetModules(const Value: TStrings);
  public
    { Public declarations }
    property Modules:TStrings read GetModules write SetModules;
  end;

var
  frmModules: TfrmModules;

procedure ShowHelp(const HelpFile:string;ActiveControl:TWinControl);

implementation
uses
  HtmlHlp; // download from http://delphi-jedi.org (API Library Files) 

{$R *.dfm}
{$I popups.inc}
resourcestring
  SOpenModuleTitle = 'Add Module for Error Searching';
  SOpenModuleFilter = 'Module files (*.exe;*.dll)|*.exe;*.dll|All files (*.*)|*.*';
  SHelpFile = 'ErrLook.chm';

procedure ShowHelp(const HelpFile:string;ActiveControl:TWinControl);
var Command,Data:integer;S:string;
begin
  Command := HH_DISPLAY_TOC;
  Data    := 0;
  S := HelpFile;
  if (ActiveControl <> nil) and (ActiveControl.HelpContext <> 0) then
  begin
    Command := HH_HELP_CONTEXT;
    Data    := ActiveControl.HelpContext;
    S := S+ '>popup';
  end;
  HtmlHelp(Application.Handle, PChar(S), Command, Data);
end;

procedure TfrmModules.alModulesUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acAdd.Enabled := edModuleName.Text <> '';
  acRemove.Enabled := lbModules.ItemIndex > -1;
end;

procedure TfrmModules.acAddExecute(Sender: TObject);
begin
  lbModules.ItemIndex := lbModules.Items.Add(edModuleName.Text);
  edModuleName.Text := '';
end;

function TfrmModules.GetModules: TStrings;
begin
  Result := lbModules.Items;
end;

procedure TfrmModules.SetModules(const Value: TStrings);
begin
  lbModules.Items := Value;
end;

procedure TfrmModules.acBrowseExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Application) do
  try
    Title := SOpenModuleTitle;
    Filter := SOpenModuleFilter;
    Filename := edModuleName.Text;
    if Execute then
      edModuleName.Text := Filename;
  finally
    Free;
  end;
end;

procedure TfrmModules.acRemoveExecute(Sender: TObject);
begin
  edModuleName.Text := lbModules.Items[lbModules.ItemIndex];
  lbModules.Items.Delete(lbModules.ItemIndex);
end;

procedure TfrmModules.acHelpExecute(Sender: TObject);
begin
  ShowHelp(SHelpFile,ActiveControl);
end;

procedure TfrmModules.SetupHelp;
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TWinControl then
      TWinControl(Components[i]).HelpContext := IDH_MODULES;
  HelpContext := IDH_MODULES;
end;

procedure TfrmModules.FormCreate(Sender: TObject);
begin
  SetupHelp;
end;

function TfrmModules.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := false;
  Result := true;
end;

end.
