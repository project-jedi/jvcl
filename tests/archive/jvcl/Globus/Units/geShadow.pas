unit geShadow;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls
  {$IFDEF GLVER_D6}, DesignIntf, DesignWindows, DesignEditors{$ELSE} {$IFDEF GLVER_D4}, dsgnintf{$ENDIF} {$ENDIF};


type  

  TglShadow_Editor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation
uses StdCtrls, glShadow;


{ TglShadow_Editor }
procedure TglShadow_Editor.ExecuteVerb(Index: Integer);
var
  sl: TStringList;
  i: integer;
  sNewName: string;
  Shadow: TglShadow;
  procedure UpdateIt(Edit: TCustomEdit);
  var
    Sh: TglShadow;
    i, NewNameIndex: integer;
    fExists: boolean;
  begin
    if sl.IndexOf(Edit.Name) <> -1 then exit;

    TEdit(Edit).BorderStyle := bsNone;
    Sh := TglShadow.Create(Component.Owner);
    Sh.Parent := Shadow.Parent;
    Sh.SetBounds(Edit.Left, Edit.Top, Edit.Width-2, Edit.Height-2);
    Sh.Shadowed := Shadow.Shadowed;
    Sh.ShadowDepth := Shadow.ShadowDepth;
    Sh.Style.Inner := Shadow.Style.Inner;
    Sh.Style.Outer := Shadow.Style.Outer;
    Sh.Control := Edit;

    NewNameIndex := 0;
    repeat
      fExists := false;
      inc(NewNameIndex);
      sNewName := 'glShadow' + IntToStr(NewNameIndex);
      for i:=0 to (Shadow.Owner as TForm).ControlCount-1 do
      begin
        fExists := CompareText((Shadow.Owner as TForm).Controls[i].Name, sNewName) = 0;
        if fExists then break;
      end;
    until not fExists;

    Sh.Name := sNewName;
  end;

begin
  case Index of
    0:
    begin
      sl := TStringList.Create;
      Shadow := Component as TglShadow;
      for i:=0 to Shadow.Parent.ControlCount-1 do
        if (Shadow.Parent.Controls[i] is TglShadow)and((Shadow.Parent.Controls[i] as TglShadow).Control <> nil) then
          sl.Add((Shadow.Parent.Controls[i] as TglShadow).Control.Name);

      for i:=0 to Shadow.Parent.ControlCount-1 do
        if Shadow.Parent.Controls[i] is TCustomEdit then
        begin
          UpdateIt(Shadow.Parent.Controls[i] as TCustomEdit);
        end;
      sl.Free;
    end;
  end;
end;

function TglShadow_Editor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Update all edit control';
  end;
end;

function TglShadow_Editor.GetVerbCount: Integer;
begin
  Result := 1;
end;


end.
