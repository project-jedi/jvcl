var
  B : TPoint;
  P: TPoint;
begin
  P := Point(0, 0);
  B := Application.FindComponent('Test').ClientToScreen(P);
  Result := B.X;
end;
