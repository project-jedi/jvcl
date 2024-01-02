unit a;

procedure main;
var
  pDlg: TForm;
  pControl: TControl;
  pPoint: TPoint;
begin
  pDlg := TForm.Create(nil);
  try
    pControl := TControl.Create(pDlg);
    try
      pControl.Parent := pDlg;
      pPoint.x := 0;
      pPoint.y := 0;
      pControl.ClientToScreen(pPoint);
    finally
      pControl.Free;
    end;
  finally
    pDlg.Free;
  end;
end;

end.
