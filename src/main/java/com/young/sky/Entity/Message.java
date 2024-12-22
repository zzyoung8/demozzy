package com.young.sky.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class Message {
    private LocalDateTime createTime;
    private String username;
    private String itemName;
}
