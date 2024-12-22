package com.young.sky.Entity;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class Result<T> {

    private boolean success;
    private T data;
    private String message;

    public static <T> Result<T> success(){
        return new Result<>(true, null, null);
    }

    public static <T> Result<T> success(T data){
        return new Result<>(true, data, null);
    }

    public static <T> Result<T> fail(String message){
        return new Result<>(false, null, message);
    }


}
