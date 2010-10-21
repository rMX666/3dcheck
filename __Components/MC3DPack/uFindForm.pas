unit uFindForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls;

type
  TActionType = (atSearch, atReplace);

  TFSearchForm = class(TForm)
    PageSearchReplace: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    EditSearch: TEdit;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    rgSearchDirection: TRadioGroup;
    btnOk: TButton;
    btnCancel: TButton;
    EditReplace: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure PageSearchReplaceChange(Sender: TObject);
  private
    function GetActionType: TActionType;
    function GetBackward: Boolean;
    function GetCaseSensitive: Boolean;
    function GetFromCursor: Boolean;
    function GetSelectedOnly: Boolean;
    function GetWholeWords: Boolean;
    procedure SetBackward(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetFromCursor(const Value: Boolean);
    procedure SetSelectedOnly(const Value: Boolean);
    procedure SetWholeWords(const Value: Boolean);
  public
    property Backward: Boolean read GetBackward write SetBackward;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property WholeWords: Boolean read GetWholeWords write SetWholeWords;
    property FromCursor: Boolean read GetFromCursor write SetFromCursor;
    property SelectedOnly: Boolean read GetSelectedOnly write SetSelectedOnly;
    property ActionType: TActionType read GetActionType;
  end;

var
  FSearchForm: TFSearchForm;

implementation

{$R *.dfm}

procedure TFSearchForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFSearchForm.btnOkClick(Sender: TObject);
begin
  with Owner as TCustomConsole do
    DoSearchReplace(EditSearch.Text, EditReplace.Text, ActionType)
end;

procedure TFSearchForm.FormCreate(Sender: TObject);
begin
  PageSearchReplace.ActivePageIndex := 0;
end;

procedure TFSearchForm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  case Msg.CharCode of
    VK_RETURN: btnOkClick(btnOk);
    VK_ESCAPE: btnCancelClick(btnCancel);
  end;
end;

procedure TFSearchForm.FormShow(Sender: TObject);
begin
  EditSearch.SetFocus;
end;

function TFSearchForm.GetActionType: TActionType;
begin
  case PageSearchReplace.ActivePageIndex of
    0: Result := atSearch;
    1: Result := atReplace;
  end;
end;

function TFSearchForm.GetBackward: Boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TFSearchForm.GetCaseSensitive: Boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TFSearchForm.GetFromCursor: Boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TFSearchForm.GetSelectedOnly: Boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TFSearchForm.GetWholeWords: Boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

procedure TFSearchForm.PageSearchReplaceChange(Sender: TObject);
begin
  Caption := TPageControl(Sender).ActivePage.Caption;
  Label1.Parent := TPageControl(Sender).ActivePage;
  EditSearch.Parent := TPageControl(Sender).ActivePage;
  gbSearchOptions.Parent := TPageControl(Sender).ActivePage;
  rgSearchDirection.Parent := TPageControl(Sender).ActivePage;
  btnOk.Parent := TPageControl(Sender).ActivePage;
  btnCancel.Parent := TPageControl(Sender).ActivePage;

  
end;

procedure TFSearchForm.SetBackward(const Value: Boolean);
begin
  case Value of
    True:  rgSearchDirection.ItemIndex := 1;
    False: rgSearchDirection.ItemIndex := 0;
  end;
end;

procedure TFSearchForm.SetCaseSensitive(const Value: Boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TFSearchForm.SetFromCursor(const Value: Boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TFSearchForm.SetSelectedOnly(const Value: Boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TFSearchForm.SetWholeWords(const Value: Boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

end.
