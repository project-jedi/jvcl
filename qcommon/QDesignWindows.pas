{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QDesignWindows.pas, released on 2004-05-14

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QDesignWindows;

interface

uses
  SysUtils, Classes, Types, QControls, QForms,
  DesignIntf, ComponentDesigner;

type
  TDesignWindow = class(TForm, IDesignWindow, IDesignNotification,
    IEditHandler, IActivatable)
  private
    FDesigner: IDesigner;
    FComponentDesigner: IComponentDesigner;
  protected
    procedure Activate; override;
    procedure Activated; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DesignerClosed(const ADesigner: IDesigner;
      AGoingDormant: Boolean); virtual;
    procedure DesignerOpened(const ADesigner: IDesigner;
      AResurrecting: Boolean); virtual;
    function EditAction(Action: TEditAction): Boolean; virtual;
    function GetEditState: TEditState; virtual;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); virtual;
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); virtual;
    procedure ItemsModified(const ADesigner: IDesigner); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); virtual;
    procedure WindowHide; virtual;
    procedure WindowShow; virtual;

    property Designer: IDesigner read FDesigner write FDesigner;
    property ComponentDesigner: IComponentDesigner read FComponentDesigner;
  end;

implementation

{ TDesignWindow }

constructor TDesignWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentDesigner := ActiveDesigner;
  RegisterDesignNotification(Self);
end;

destructor TDesignWindow.Destroy;
begin
  FComponentDesigner := nil;
  UnregisterDesignNotification(Self);
  inherited Destroy;
end;

procedure TDesignWindow.Activate;
begin
  inherited Activate;
  Activated;
end;

procedure TDesignWindow.Activated;
begin
  // do nothing
end;

procedure TDesignWindow.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
  // do nothing
end;

procedure TDesignWindow.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin
  // do nothing
end;

function TDesignWindow.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

function TDesignWindow.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TDesignWindow.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
  // do nothing
end;

procedure TDesignWindow.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
  // do nothing
end;

procedure TDesignWindow.ItemsModified(const ADesigner: IDesigner);
begin
  // do nothing
end;

procedure TDesignWindow.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
  // do nothing
end;

procedure TDesignWindow.WindowHide;
begin
  Hide;
end;

procedure TDesignWindow.WindowShow;
begin
  Show;
end;

end.
