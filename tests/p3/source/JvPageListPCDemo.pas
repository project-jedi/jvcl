unit JvPageListPCDemo;

interface
uses
  Windows, SysUtils, Classes, JvComCtrls, JvPageListTreeView;

type
// a simple example of how to implement the IPageList interface in a TPageControl
  TJvPageListPageControl = class(TJvPageControl, IPageList)
  private
    function IPageList.CanChange = PageListCanChange;
    procedure IPageList.SetActivePageIndex = PageListSetActivePageIndex;

    function PageListCanChange(AIndex: Integer): Boolean;
    function getPageCaption(AIndex: Integer): String;
    function getPageCount: Integer;
    procedure PageListSetActivePageIndex(AIndex: Integer);
  end;

// procedure Register;

implementation

{
procedure Register;
begin
  RegisterComponents('JVCL',[TJvPageListPageControl]);
end;
}
{ TJvPageListPageControl }

function TJvPageListPageControl.getPageCaption(AIndex: Integer): String;
begin
  if (AIndex < 0) or (AIndex >= getPageCount) then
    Result := ''
  else
  begin
    Result := Pages[AIndex].Caption;
    if Result = '' then
      Result := Pages[AIndex].Name;
  end;
end;

function TJvPageListPageControl.getPageCount: Integer;
begin
  Result := PageCount;
end;

function TJvPageListPageControl.PageListCanChange(
  AIndex: Integer): Boolean;
begin
  Result := CanShowTab(AIndex);
end;

procedure TJvPageListPageControl.PageListSetActivePageIndex(
  AIndex: Integer);
begin
  ActivePageIndex := AIndex;
end;

end.

