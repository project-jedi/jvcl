unit geHelpPanel;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls
  {$IFDEF GLVER_D6}, DesignIntf, DesignWindows, DesignEditors{$ELSE} {$IFDEF GLVER_D4}, dsgnintf{$ENDIF} {$ENDIF};


type  
  TglHelpPanel_Editor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation
uses glHelpPanel, geRTFPreview;
{ TglHelpPanel_Editor }

procedure TglHelpPanel_Editor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  ms: TMemoryStream;
begin
  inherited;
  case Index of
    0:
    begin
      OpenDialog := TOpenDialog.Create(nil);
      OpenDialog.Filter := 'RTF and Text files (*.rtf,*.txt)|*.rtf;*.txt';
      if OpenDialog.Execute then
      begin
        (Component as TglHelpPanel).Strings.LoadFromFile(OpenDialog.FileName);
      end;
      OpenDialog.Free;
    end;
    1:
    begin
      try
        fRTFPreview := TfRTFPreview.Create(nil);

        ms := TMemoryStream.Create;
        try
          (Component as TglHelpPanel).Strings.SaveToStream(ms);
          ms.Position := 0;
          fRTFPreview.Rich.Lines.LoadFromStream(ms);
          fRTFPreview.ShowModal;
        finally
          ms.Free;
        end;

      finally
        fRTFPreview.Free;
      end;
    end;
  end;
end;

function TglHelpPanel_Editor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Load RTF file';
    1: Result := 'Preview RTF text';
  end;
end;

function TglHelpPanel_Editor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
