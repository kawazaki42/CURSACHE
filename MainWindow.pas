unit MainWindow;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars} // CSV

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Menus,
  ActnList, StdActns, ExtCtrls, DateTimePicker,

  LCSVUtils, DateUtils,
  DBRecord, ReportView;

type

  { TFormMain }

  TFormMain = class(TForm)
    Action1: TAction;
    ActionList1: TActionList;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonToggleEdit: TButton;
    ButtonDup: TButton;
    DateTimePicker1: TDateTimePicker;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid: TStringGrid;
    ToggleBox1: TToggleBox;
    procedure Action1Execute(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonToggleEditClick(Sender: TObject);
    procedure ButtonDupClick(Sender: TObject);
    procedure ButtonEditSaveClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGridHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGridResize(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);

  private

  public

  end;

var
  FormMain: TFormMain;

implementation

type
  DBColumn = (colID = 0, colTitle, colAuthor, colCard, colReader, colDate);
  StoredFields = (fTitle = 0, fAuthor, fCard, fReader, fDate);

var
  PEditedRecord: PItem;
  SearchMode: Boolean;
  storage: TRecordList;
  filtered: TRecordList;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  storage := TRecordList.Create;
  filtered := TRecordList.Create;
  PEditedRecord := nil;
  SearchMode := false;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  storage.free;
  filtered.free;
end;

procedure TFormMain.StringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  a, b: TDateTime;
  astr, bstr: string;
begin
  result := 0;
  astr := StringGrid.cells[ACol, ARow];
  bstr := StringGrid.cells[BCol, BRow];
  if (acol = Ord(colDate)) and (bcol = Ord(colDate)) then
  begin
    //a := StrToDate(StringGrid.cells[ACol, ARow]);
    //b := StrToDate(StringGrid.cells[BCol, BRow]);
    if not trystrtodate(astr, a) then exit;
    if not trystrtodate(bstr, b) then exit;
    result := CompareDate(a, b);
  end else
  begin
    result := AnsiCompareText(astr, bstr);
  end;
  if StringGrid.SortOrder = soDescending then
    result := -result;
end;

procedure TFormMain.StringGridHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  e: TControl;
begin
  case index of
    Ord(colTitle): e := edit1;
    Ord(colAuthor): e := edit2;
    Ord(colCard): e := edit3;
    Ord(colReader): e := edit4;
    Ord(colDate): e := DateTimePicker1;
  end;
  //e.width := StringGrid.ColWidths[index];
  with e.Constraints do
  begin
    MinWidth := StringGrid.ColWidths[index];
    MaxWidth := MinWidth;
  end;
end;

procedure TFormMain.StringGridResize(Sender: TObject);
begin
  StringGrid.AutoFillColumns := true;
  StringGrid.AutoFillColumns := false;
end;

procedure Redisplay(List: TRecordList);
var
  rec: TRecord;
  i: integer;
  OldRow: integer;
begin
  //FormMain.stringgrid1.Clear;
  OldRow := FormMain.StringGrid.Row;

  //FormMain.StringGrid1.Clean([gzNormal]);
  FormMain.StringGrid.RowCount := 1;
  i := 1;
  for rec in List do
  begin
    with FormMain.StringGrid do
      RowCount := RowCount + 1;

    FormMain.StringGrid.Rows[i][ord(colID)] := i.tostring;
    FormMain.StringGrid.Rows[i][ord(colTitle)] := rec.name;
    FormMain.StringGrid.Rows[i][ord(colAuthor)] := rec.author;
    FormMain.StringGrid.Rows[i][ord(colCard)] := rec.cardID;
    FormMain.StringGrid.Rows[i][ord(colReader)] := rec.reader;
    FormMain.StringGrid.Rows[i][ord(colDate)] := datetostr(rec.returndate);

    inc(i);
  end;

  FormMain.StringGrid.Row := OldRow;
end;

function SelectedRecord: TRecordList.PItem;
begin
  result := GetItem(storage, FormMain.StringGrid.Row);
end;

function RecordFromInput: TRecord;
begin
  result.name := FormMain.Edit1.Text;
  result.author := FormMain.Edit2.Text;
  result.cardID := FormMain.Edit3.Text;
  result.reader := FormMain.Edit4.Text;
  result.returndate := FormMain.DateTimePicker1.date;
end;

procedure ClearInput;
begin
  FormMain.Edit1.Clear;
  FormMain.Edit2.Clear;
  FormMain.Edit3.Clear;
  FormMain.Edit4.Clear;
  FormMain.DateTimePicker1.date := today;
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  //x.name := Edit1.Text;
  //x.author := Edit2.Text;
  //x.cardID := Edit3.Text;
  //x.reader := Edit4.Text;
  //x.returndate := DateTimePicker1.date;
  //x.returndate := ;
  if PEditedRecord <> nil then
    ButtonEditSaveClick(self)
  else
    storage.InsertLast( RecordFromInput );
  redisplay(storage);
  ClearInput;
end;

procedure TFormMain.Action1Execute(Sender: TObject);
begin
  FormReport.show;
  ShowReport(storage);
end;

procedure TFormMain.ButtonDeleteClick(Sender: TObject);
{var
  cur: TRecordList.PItem;
  i, offset: integer;}
begin
  {cur := storage.First;
  offset := StringGrid.row - 1;
  for i := 1 to offset do
    cur := cur^.next;}
  if storage.Count = 0 then exit;

  storage.delete( SelectedRecord );
  redisplay(storage);
end;

procedure EnterEditMode;
begin
  //FormMain.ButtonEditSave.Enabled := true;
  //FormMain.Button6.Enabled := true;

  //FormMain.ButtonAdd.Enabled := false;
  FormMain.ButtonAdd.Caption := 'Сохранить';
  FormMain.ButtonToggleEdit.Caption := 'Отмена';
  FormMain.ButtonDelete.Enabled := false;
  FormMain.ButtonDup.Enabled := false;
end;

procedure ExitEditMode;
begin
  PEditedRecord := nil;
  //FormMain./ButtonEditSave.Enabled := false;
  //FormMain.button6.Enabled := false;

  //FormMain.ButtonAdd.Enabled := true;
  FormMain.ButtonAdd.Caption := 'Добавить';
  FormMain.ButtonToggleEdit.Caption := 'Редактировать';
  FormMain.ButtonDelete.Enabled := true;
  FormMain.ButtonDup.Enabled := true;
end;


// Переключить режим редактирования
procedure TFormMain.ButtonToggleEditClick(Sender: TObject);
//var
  //item: TRecordList.PItem;
begin
  if storage.Count = 0 then exit;

  // Режим редактирования активен, отключить
  if PEditedRecord <> nil then
  begin
    ExitEditMode;
    ClearInput;
    exit;
  end;

  PEditedRecord := SelectedRecord;
  Edit1.Text := PEditedRecord^.data.name;
  Edit2.Text := PEditedRecord^.data.author;
  Edit3.Text := PEditedRecord^.data.CardID;
  Edit4.Text := PEditedRecord^.data.Reader;
  DateTimePicker1.date := PEditedRecord^.data.returndate;
  EnterEditMode;
  {x.author := Edit2.Text;
  x.cardID := Edit3.Text;
  x.reader := Edit4.Text;
  x.returndate := DateTimePicker1.date;}
  //redisplay;
end;

procedure TFormMain.ButtonDupClick(Sender: TObject);
//var
//  OldRow: integer;
begin
  if storage.Count = 0 then exit;

  SelectedRecord^.InsertAfter(SelectedRecord^.data);
  //OldRow := StringGrid.Row;
  redisplay(storage);
  StringGrid.Row := StringGrid.row + 1;
end;

procedure TFormMain.ButtonEditSaveClick(Sender: TObject);
begin
  if PEditedRecord = nil then exit;
  PEditedRecord^.data := RecordFromInput;
  redisplay(storage);
  //ButtonEditSave.Enabled := false;
  ExitEditMode;
end;

procedure TFormMain.Button6Click(Sender: TObject);
begin
  ExitEditMode;
end;

function FilterRec(rec: TRecord): Boolean;
var
  //edit: TEdit;
  Filter: TRecord;
begin
  Filter := RecordFromInput;
  result := true;
  //with FormMain.Edit1 do
  {for edit in [FormMain.Edit1, FormMain.Edit2, FormMain.Edit3, FormMain.Edit4] do
    if edit.Text <> '' then
     //with rec.name do
      if not Pos(edit.Text, rec. then
        result := false;
  if FormMain.DateTimePicker1.date <> rec.returndate then
    result := false;}
  if Filter.name <> '' then
    if Pos(filter.name, rec.name) = 0 then
      result := false;

  if Filter.author <> '' then
    if Pos(filter.author, rec.author) = 0 then
      result := false;

  if Filter.reader <> '' then
    if Pos(filter.reader, rec.reader) = 0 then
      result := false;

  if Filter.cardID <> '' then
    if Pos(filter.cardID, rec.cardID) = 0 then
      result := false;
end;

procedure TFormMain.ToggleBox1Change(Sender: TObject);
var
  rec: TRecord;
begin
  //SearchMode := ToggleBox1.Checked;
  //Filter := RecordFromInput;
  filtered.Clear;
  for rec in storage do
    if FilterRec(rec) then
      filtered.InsertLast(rec);
  redisplay(filtered);
end;

procedure LoadRecord(Fields: TStringList);
var
  x: TRecord;
begin
  x.name := Fields[Ord(fTitle)];
  x.author := Fields[Ord(fAuthor)];
  x.cardID := Fields[Ord(fCard)];
  x.reader := Fields[Ord(fReader)];
  x.returndate := strtodate(Fields[Ord(fDate)]);
  //x.returndate := ;
  storage.InsertLast(x);
end;

procedure TFormMain.FileOpen1Accept(Sender: TObject);
//const
  //handler: TCSVRecordProc = LoadRecord;

{procedure handler(Fields: TStringList);
begin
  LoadRecord(Fields);
end;}

//const haddr: TCSVRecordProc = handler;

begin
  storage.Clear;
  with FileOpen1.Dialog do
    if FileName.EndsWith('.TXT', true) or FileName.EndsWith('.CSV', true) then
      LoadFromCSVFile(FileName, @LoadRecord)
    else
      LoadFromBinaryFile(storage, filename);
  redisplay(storage);
end;

procedure TFormMain.FileSaveAs1Accept(Sender: TObject);
begin
  with FileSaveAs1.Dialog do
    if FileName.EndsWith('.TXT', true) or FileName.EndsWith('.CSV', true) then
      StringGrid.SaveToCSVFile(FileName, ',', false)
    else
      SaveToBinaryFile(storage, filename);
end;

end.

