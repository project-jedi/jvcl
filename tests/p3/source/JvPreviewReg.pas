unit JvPreviewReg;

interface
uses
  DesignEditors, DesignIntf;

type
  TJvPreviewerEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation
uses
  Classes, JvPrvwDoc, JvPrvwRender;

procedure Register;
begin
  RegisterComponents('Jv Preview', [TJvPreviewControl,
    TJvRichEditPreviewer, TJvStringsPreviewer,
      TJvGraphicPreviewer, TJvControlPreviewer, TJvPreviewPrinter]);
//  RegisterComponentEditor(TJvCustomPreviewer, TJvPreviewerEditor);
end;

type
  TJvHackCustomPreviewer = class(TJvCustomPreviewer);

  { TJvPreviewerEditor }

procedure TJvPreviewerEditor.ExecuteVerb(Index: Integer);
var pv: TJvCustomPreviewControl;
begin
  case Index of
    0:
      TJvCustomPreviewer(Component).CreatePreview(false);
    1:
      begin
        pv := TJvHackCustomPreviewer(Component).PrintPreview;
        if pv <> nil then
          pv.Clear;
      end;
  end;
end;

function TJvPreviewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Create Preview';
    1:
      Result := 'Clear Preview';
  end;
end;

function TJvPreviewerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

