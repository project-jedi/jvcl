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

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDotNetControls;

interface

uses
  Forms, Classes, Windows, Messages, Graphics, Controls,
  StdCtrls, ComCtrls, Mask,
  {$IFDEF USEJVCL}
  CheckLst,
  JVCLVer;
  {$ELSE}
  CheckLst;
  {$ENDIF USEJVCL}

type
  TJvDotNetCheckListBox = class(TCheckListBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetEdit = class(TEdit)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetHotKey = class(THotKey)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetListBox = class(TListBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetListView = class(TListView)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetMaskEdit = class(TMaskEdit)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetMemo = class(TMemo)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetRichEdit = class(TRichEdit)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetScrollBox = class(TScrollBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetTreeView = class(TTreeView)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
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

//=== TJvDotNetCheckListBox ==================================================

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

//=== TJvDotNetHotKey ========================================================

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

//=== TJvDotNetListBox =======================================================

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

//=== TJvDotNetListView ======================================================

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

//=== TJvDotNetMaskEdit ======================================================

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

//=== TJvDotNetMemo ==========================================================

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

//=== TJvDotNetRichEdit ======================================================

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

//=== TJvDotNetScrollBox =====================================================

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

//=== TJvDotNetTreeView ======================================================

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

end.
