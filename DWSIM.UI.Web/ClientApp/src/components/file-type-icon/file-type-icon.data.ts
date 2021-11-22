
export interface ICustomFileTypeToIconMap{
    [key: string]: { extensions?: string[], imageUrl:string } 
}
export const CustomFileTypeToIconMap:ICustomFileTypeToIconMap = { 
    dwxml: {
        extensions: ['dwxml', 'dwxmz'],
        imageUrl:require("../../assets/icons/file-types/dwsim.png")
    },
    folder:{
        extensions:['folder'],
        imageUrl:require("../../assets/icons/file-types/folder.svg")
    }
  
};