{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDotNetCtrls.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are
Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDotNetControls;

interface

uses
  Windows, Messages, Classes, Controls,
  {$IFDEF USEJVCL}
  JvRichEdit, JvListView, JvCheckListBox, JvEdit, JvHotKey, JvListBox,
  JvMaskEdit, JvMemo, JvComCtrls, JvScrollBox, JvToolEdit, JVCLVer,
  {$ELSE}
  Forms, CheckLst, ComCtrls, Mask,
  {$ENDIF USEJVCL}
  StdCtrls;

type
  {$IFDEF USEJVCL}
  TJvDotNetCheckListBox = class(TJvCheckListBox)
  {$ELSE}
  TJvDotNetCheckListBox = class(TCheckListBox)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetEdit = class(TJvEdit)
  {$ELSE}
  TJvDotNetEdit = class(TEdit)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetHotKey = class(TJvHotKey)
  {$ELSE}
  TJvDotNetHotKey = class(THotKey)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetListBox = class(TJvListBox)
  {$ELSE}
  TJvDotNetListBox = class(TListBox)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetListView = class(TJvListView)
  {$ELSE}
  TJvDotNetListView = class(TListView)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetMaskEdit = class(TJvMaskEdit)
  {$ELSE}
  TJvDotNetMaskEdit = class(TMaskEdit)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetMemo = class(TJvMemo)
  {$ELSE}
  TJvDotNetMemo = class(TMemo)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetRichEdit = class(TJvRichEdit)
  {$ELSE}
  TJvDotNetRichEdit = class(TRichEdit)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}
  TJvDotNetScrollBox = class(TJvScrollBox)
  {$ELSE}
  TJvDotNetScrollBox = class(TScrollBox)
 {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {$IFDEF USEJVCL}
  TJvDotNetTreeView = class(TJvTreeView)
  {$ELSE}
  TJvDotNetTreeView = class(TTreeView)
  {$ENDIF USEJVCL}
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF USEJVCL}

  TJvDotNetFilenameEdit = class(TJvFilenameEdit)
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDotNetDirectoryEdit = class(TJvDirectoryEdit)
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$ENDIF USEJVCL}

  TJvDotNetButton = class(TButton)
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
  end;

(* TJvDotNetCustomControl = class(TWinControl)
  published
    { Published declarations }
    property Color;
  end;
*)

implementation

uses
  JvDotNetUtils;

//=== { TJvDotNetCheckListBox } ==============================================

constructor TJvDotNetCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetCheckListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetCheckListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetEdit } ======================================================

constructor TJvDotNetEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetHotKey } ====================================================

constructor TJvDotNetHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetHotKey.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetHotKey.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetListBox } ===================================================

constructor TJvDotNetListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetListView } ==================================================

constructor TJvDotNetListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetListView.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetListView.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetMaskEdit } ==================================================

constructor TJvDotNetMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetMaskEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetMaskEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetMemo } ======================================================

constructor TJvDotNetMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetMemo.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetMemo.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetRichEdit } ==================================================

constructor TJvDotNetRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetRichEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetRichEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetScrollBox } =================================================

constructor TJvDotNetScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetScrollBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetScrollBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetTreeView } ==================================================

constructor TJvDotNetTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetTreeView.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetTreeView.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

{$IFDEF USEJVCL}

//=== { TJvDotNetFilenameEdit } ==============================================

constructor TJvDotNetFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetFilenameEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetFilenameEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDirectoryEdit } =============================================

constructor TJvDotNetDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDirectoryEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDirectoryEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

{$ENDIF USEJVCL}

//=== { TJvDotNetButton } ====================================================

constructor TJvDotNetButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetButton.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetButton.InternalWindowProc(var Msg: TMessage);
begin
  // (p3) this doesn't work 100% when tabbing into the button
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
  if Msg.Msg = CM_MOUSELEAVE then
    Invalidate; // redraw 3D border
end;

end.
