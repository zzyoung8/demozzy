package com.young.sky.Controller;

import com.young.sky.Entity.Item;
import com.young.sky.Service.ItemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;


@RestController
@RequestMapping("/production")
public class ItemController {

    @Resource
    private ItemService itemService;

    @GetMapping("/items")
    public List<Item> getAllItem(){
        return itemService.getAllItem();
    }
}
