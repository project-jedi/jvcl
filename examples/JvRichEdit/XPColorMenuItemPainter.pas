unit XPColorMenuItemPainter;

interface

uses Windows, Menus, JvMenus;

type

  // This show how easy it is to derive a class from an existing
  // painter and use it with the menu items.
  TJvXPColorMenuItemPainter = class (TJvXPMenuItemPainter)
  public
    procedure Paint(Item : TMenuItem;
                    ItemRect : TRect;
                    State: TMenuOwnerDrawState); override;
  end;

implementation

uses Graphics;

{ TJvXPColorMenuItemPainter }

procedure TJvXPColorMenuItemPainter.Paint(Item : TMenuItem;
                ItemRect : TRect;
                State: TMenuOwnerDrawState);
begin
  // make xp painter paint the item
  inherited;

  // add our colored square
  ItemRect.Left := ItemRect.Left + 5;
  ItemRect.Right := ItemRect.Left + 12;
  ItemRect.Top := ItemRect.Top + 6;
  ItemRect.Bottom := ItemRect.Top + 13;
  with Canvas do
  begin
    Brush.Color := clMenuText;
    FrameRect(ItemRect);
    InflateRect(ItemRect, -1, -1);
    Brush.Color := Item.Tag;
    FillRect(ItemRect);
  end;
end;

end.
