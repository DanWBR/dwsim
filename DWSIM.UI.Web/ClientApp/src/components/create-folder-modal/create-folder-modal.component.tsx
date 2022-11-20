import { DefaultButton, Dialog, DialogFooter, DialogType, IModalProps, PrimaryButton, TextField } from "@fluentui/react";
import * as React from "react";
import { useForm } from "react-hook-form";
import { CreateFolder } from "../../api/documents.api";
import { ISelectedFolder } from "../../interfaces/documents/document.interfaces";

interface ICreateFolderModalProps {
   selectedFolder:ISelectedFolder;
   flowsheetsDriveId:string;
    onHide():void;
    onFolderCreated():void;
}


const modelProps = {
    isBlocking: true,
    topOffsetFixed: true

} as IModalProps;

const dialogContentProps = {
    type: DialogType.normal,
    title: 'Create Folder'
   
  };

const CreateFolderModal: React.FC<ICreateFolderModalProps> = (props) => {



    
    const { register, handleSubmit, watch, formState: { errors } } = useForm();
    const onSubmit = async (data:any) => {
        
     await   CreateFolder(data.foldername,props.flowsheetsDriveId,props.selectedFolder.driveId);
     props.onFolderCreated();
    }
console.log("errors",errors);
    return <Dialog
        hidden={false}
        dialogContentProps={dialogContentProps}
        onDismiss={props.onHide}
        minWidth={"350px"}
    
        modalProps={modelProps}>
        <form onSubmit={handleSubmit(onSubmit)}>

            <TextField  {...register("foldername", { required: true })} placeholder="Folder name"/>
            {errors.foldername && <span className="text-danger" style={{margin:"3px 5px"}}>Folder name is required.</span>}

            <DialogFooter>
                <PrimaryButton type="submit" text="Create" />
                <DefaultButton onClick={props.onHide} text="Cancel" />
            </DialogFooter>
        </form>
    </Dialog>







}


export default CreateFolderModal