unit AlignFnc;

interface
uses Controls, glTypes;

type
TNeedAlign = function(Control: TControl): boolean;

procedure AlignControlsInWindow( Wnd: TWinControl; NeedAlign: TNeedAlign; HCAlign: TglHComponentAlign; VCAlign: TglVComponentAlign );

implementation
uses Windows, Classes, glUtils;

var
  fHorizSort: boolean;
  c: TControl;
procedure AlignControlsInWindow( Wnd: TWinControl; NeedAlign: TNeedAlign; HCAlign: TglHComponentAlign; VCAlign: TglVComponentAlign );
var
  i, TotalControls, ControlNo: integer;
  R: TRect;
  TotalSize, AccumulatedSize: TSize;
  ControlsList: TList;
  Control: TControl;
  procedure Sort(fHorizSort: boolean);
  var
    i: integer;
    fSorted: boolean;
    Control2: TControl;
  begin
    repeat
      fSorted := true;
      for i := 0 to ControlsList.Count - 2 do
      begin
        Control := ControlsList[i]; Control2 := ControlsList[i+1];
        if fHorizSort then
        begin
          if Control.Left > Control2.Left then
          begin ControlsList.Exchange(i,i+1); fSorted := false; end;
        end else
          if Control.Top > Control2.Top then
          begin ControlsList.Exchange(i,i+1); fSorted := false; end;
      end;
    until fSorted;
  end;
begin
  if not Assigned(Wnd) then exit;
  ControlsList := TList.Create;
  try
    R := Rect(65536,65536,0,0);
    TotalSize.cx := 0; TotalSize.cy := 0;
    AccumulatedSize.cx := 0; AccumulatedSize.cy := 0;
    TotalControls := -1; ControlNo := 0;
    with Wnd do//...calc sizes and sort controls
    for i:=0 to ControlCount-1 do
      if NeedAlign(Controls[i]) then with Controls[i] do
      begin
        R.Left := min( R.Left, Left );
        R.Top := min( R.Top, Top );
        R.Right := max( R.Right, Left+Width );
        R.Bottom := max( R.Bottom, Top+Height );
        inc( TotalSize.cx, Width );
        inc( TotalSize.cy, Height );
        inc( TotalControls );
        ControlsList.Add( Controls[i] );
      end;

    Sort( true );
    //..h aligning
    for i:=0 to ControlsList.Count-1 do with Control do
    begin
      Control := ControlsList[i];
      case HCAlign of
        haLeft: Left := R.Left;
        haCenters: Left := R.Left + (R.Right - R.Left - Width) div 2;
        haRight: Left := R.Right - Width;
        haSpaceEqually: if ControlNo <> TotalControls then Left := R.Left + AccumulatedSize.cx+trunc((R.Right - R.Left - TotalSize.cx)/TotalControls * ControlNo);
        haCenterWindow: Left := (Wnd.Width - Width) div 2;
        haClose: Left := R.Left + AccumulatedSize.cx;
      end;
      inc( AccumulatedSize.cx, Width );
      inc( ControlNo );
    end;
    ControlNo := 0;
    Sort( false );
    //..v aligning
    for i:=0 to ControlsList.Count-1 do with Control do
    begin
      Control := ControlsList[i];
      case VCAlign of
        vaTops: Top := R.Top;
        vaCenters: Top := R.Top + (R.Bottom - R.Top - Height) div 2;
        vaBottoms: Top := R.Bottom - Height;
        vaSpaceEqually: if ControlNo <> TotalControls then Top := R.Top + AccumulatedSize.cy+trunc((R.Bottom - R.Top - TotalSize.cy)/TotalControls * ControlNo);
        vaCenterWindow: Top := (Wnd.Height - Height) div 2;
        vaClose: Top := R.Top + AccumulatedSize.cy;        
      end;
      inc( AccumulatedSize.cy, Height );
      inc( ControlNo );
    end;

  finally
    ControlsList.Free;
  end;
end;

end.
 