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
    pControl.Parent := pDlg;
    pPoint.x := 0;
    pPoint.y := 0;
    pControl.ClientToScreen(pPoint);
    pDlg.ShowModal;
  finally
    pDlg.Free;
  end;
end;

end.
