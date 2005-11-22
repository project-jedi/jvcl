//---------------------------------------------------------------------------

#ifndef RegTVMainFormUH
#define RegTVMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvExComCtrls.hpp"
#include "JvRegistryTreeView.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TRegTVMainForm : public TForm
{
__published:	// IDE-managed Components
        TListView *ListView1;
        TSplitter *Splitter1;
        TJvRegistryTreeView *RegistryTreeView1;
        TMainMenu *mmMain;
        TMenuItem *Registry1;
        TMenuItem *Importregistryfile1;
        TMenuItem *Exportregistryfile1;
        TMenuItem *N1;
        TMenuItem *Connectnetworkdrive1;
        TMenuItem *Disconnectnetworkdrive1;
        TMenuItem *N2;
        TMenuItem *Print1;
        TMenuItem *N3;
        TMenuItem *Exit1;
        TMenuItem *Edit1;
        TMenuItem *New1;
        TMenuItem *Key1;
        TMenuItem *N4;
        TMenuItem *Stringvalue1;
        TMenuItem *Binaryvalue1;
        TMenuItem *DWORDvalue1;
        TMenuItem *N5;
        TMenuItem *Delete1;
        TMenuItem *Rename1;
        TMenuItem *N6;
        TMenuItem *Copykeyname1;
        TMenuItem *N7;
        TMenuItem *Find1;
        TMenuItem *FindNext1;
        TMenuItem *View1;
        TMenuItem *Statusbar2;
        TMenuItem *N8;
        TMenuItem *Refresh1;
        TMenuItem *Favorites1;
        TMenuItem *Addtofavorites1;
        TMenuItem *Deletefavorite1;
        TMenuItem *N10;
        TMenuItem *Help1;
        TMenuItem *HelpIndex1;
        TMenuItem *N11;
        TMenuItem *AboutRegistryeditordemo1;
        TActionList *alMain;
        TAction *acImport;
        TAction *acExport;
        TAction *acConnectNetwork;
        TAction *acDisconnectNetwork;
        TAction *acPrint;
        TAction *acExit;
        TAction *acNewKey;
        TAction *acNewString;
        TAction *acNewBinary;
        TAction *acNewDWORD;
        TAction *acDelete;
        TAction *acRename;
        TAction *acCopyName;
        TAction *acFind;
        TAction *acFindNext;
        TAction *acStatusBar;
        TAction *acRefresh;
        TAction *acAddFav;
        TAction *acDelFav;
        TAction *acHelp;
        TAction *acAbout;
        TStatusBar *StatusBar1;
        void __fastcall ListView1Change(TObject *Sender, TListItem *Item,
          TItemChange Change);
        void __fastcall RegistryTreeView1Expanded(TObject *Sender,
          TTreeNode *Node);
        void __fastcall RegistryTreeView1Expanding(TObject *Sender,
          TTreeNode *Node, bool &AllowExpansion);
        void __fastcall Importregistryfile1Click(TObject *Sender);
        void __fastcall Exportregistryfile1Click(TObject *Sender);
        void __fastcall Connectnetworkdrive1Click(TObject *Sender);
        void __fastcall Disconnectnetworkdrive1Click(TObject *Sender);
        void __fastcall Exit1Click(TObject *Sender);
        void __fastcall Key1Click(TObject *Sender);
        void __fastcall Delete1Click(TObject *Sender);
        void __fastcall Rename1Click(TObject *Sender);
        void __fastcall Statusbar2Click(TObject *Sender);
        void __fastcall Refresh1Click(TObject *Sender);
        void __fastcall Addtofavorites1Click(TObject *Sender);
        void __fastcall AboutRegistryeditordemo1Click(TObject *Sender);
private:
        void __fastcall DoFavoriteClick(TObject * Sender);	// User declarations
public:		// User declarations
        __fastcall TRegTVMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRegTVMainForm *RegTVMainForm;
//---------------------------------------------------------------------------
#endif
