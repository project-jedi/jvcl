unit JvSLDMappingEditorDialog;

{$I JVCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvBaseDlg, JvSegmentedLEDDisplay, JvSegmentedLEDDisplayMapperFrame;

type
  TfrmSLDMappingEditorDialog = class(TForm)
    EditorFrame: TfmeJvSegmentedLEDDisplayMapper;
    lblDigitClassCaption: TLabel;
    lblSegmentCountCaption: TLabel;
    lblCharCaption: TLabel;
    lblMapperValueCaption: TLabel;
    lblSegmentsCaption: TLabel;
    lblDigitClass: TLabel;
    lblSegmentCount: TLabel;
    lblChar: TLabel;
    lblMapperValue: TLabel;
    lblSegments: TLabel;
    btnOK: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure UpdateDigitClass(Sender: TObject);
    procedure UpdateInfo(Sender: TObject);
  public
    { Public declarations }
  end;

procedure SegmentedLEDDisplayMappingEditor(ADisplay: TJvCustomSegmentedLEDDisplay; var OpenFolder,
  SaveFolder: string);

implementation

{$R *.DFM}

procedure SegmentedLEDDisplayMappingEditor(ADisplay: TJvCustomSegmentedLEDDisplay; var OpenFolder,
  SaveFolder: string);
begin
  with TfrmSLDMappingEditorDialog.Create(Application) do
  try
    EditorFrame.Display := ADisplay;
    EditorFrame.LastOpenFolder := OpenFolder;
    EditorFrame.LastSaveFolder := SaveFolder;
    ShowModal;
    OpenFolder := EditorFrame.LastOpenFolder;
    SaveFolder := EditorFrame.LastSaveFolder;
  finally
    Free;
  end;
end;

//===TfrmSLDMappingEditorDialog=====================================================================

procedure TfrmSLDMappingEditorDialog.Loaded;
begin
  inherited Loaded;
  EditorFrame.OnDisplayChanged := UpdateDigitClass;
  EditorFrame.OnInfoUpdate := UpdateInfo;
end;

procedure TfrmSLDMappingEditorDialog.UpdateDigitClass(Sender: TObject);
begin
  if EditorFrame.Display <> nil then
  begin
    lblDigitClass.Caption := EditorFrame.DigitClass.ClassName;
    lblSegmentCount.Caption := IntToStr(EditorFrame.DigitClass.SegmentCount);
  end
  else
  begin
    lblDigitClass.Caption := '';
    lblSegmentCount.Caption := '';
  end;
end;

procedure TfrmSLDMappingEditorDialog.UpdateInfo(Sender: TObject);
begin
  with EditorFrame do
  begin
    if CharSelected then
    begin
      if CurChar in ['!' .. 'z'] then
        lblChar.Caption := CurChar + ' (#' + IntToStr(Ord(CurChar)) + ')'
      else
        lblChar.Caption := '#' + IntToStr(Ord(CurChar));
    end
    else
      lblChar.Caption := '';
    if Display <> nil then
    begin
      lblMapperValue.Caption := IntToStr(sldEdit.Digits[0].GetSegmentStates);
      lblSegments.Caption := sldEdit.Digits[0].GetSegmentString;
    end
    else
    begin
      lblMapperValue.Caption := '';
      lblSegments.Caption := '';
    end;
  end;
end;

procedure TfrmSLDMappingEditorDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := EditorFrame.CanClose;
end;

end.
